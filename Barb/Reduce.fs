module Barb.Reduce

open System
open System.Collections
open System.Collections.Concurrent

open Barb.Interop
open Barb.Representation

let indexIntoTuple (elements: ExprTypes array) (index: obj) = 
    match index with 
    | :? int64 as idx -> elements.[int idx] 
    | _ -> failwith (sprintf "Bad type for tuple index: %A" index)

let tupleToObj (tuple: ExprRep array) = 
    tuple |> Array.map (fun ex -> ex.Expr) 
    |> Array.map (function | Obj v -> v | other -> failwith (sprintf "Cannot resolve the given tuple-internal expression to a object type: %A" other))

let resolveExpressionResult (input: ExprRep list) =
    match input with
    | { Expr = Obj (res) } :: [] -> res
    | { Expr = Tuple (items) } :: [] -> tupleToObj items |> box
    | otherTokens -> failwith (otherTokens |> List.fold (fun s t -> s + (sprintf "Unexpected result: %A" t.Expr)) "")

let applyArgToLambda (l: LambdaRecord) (arg: obj) =
    match l.Params with
    | [] -> failwith (sprintf "Unexpected Lambda Argument %A" arg)
    | bindname :: restprms ->
        let bindings = l.Bindings |> Map.add bindname (Obj arg |> Lazy.CreateFromValue)
        Lambda({ l with Params = restprms; Bindings = bindings })

let resolveExpression exprs initialBindings settings (finalReduction: bool) = 

    let rec (|ResolveSingle|_|) bindings =
        function 
        | left, ({ Expr = rExpr } & r) :: rt ->
            let wrapit e = { r with Expr = e }
            match rExpr with
            | Returned o -> resolveResultType o |> wrapit |> Some
            | SubExpression exp ->
                match reduceExpressions [] exp bindings |> List.rev with
                | single :: [] -> single
                | many -> SubExpression many |> Unresolved |> wrapit
                |> Some
            | IndexArgs ({ Expr = SubExpression exp }) when finalReduction ->
                match reduceExpressions [] exp bindings |> List.rev with
                | [] -> failwith (sprintf "No indexer found in [ ]")
                | single :: [] -> IndexArgs single |> wrapit
                | other -> failwith (sprintf "Indexer could not be fully reduced.")
                |> Some
            | Tuple tc -> 
                let reducedTuples = tc |> Array.map (fun t -> match reduceExpressions [] [t] bindings with | ({ Expr = Obj o } & oobj) :: [] -> oobj 
                                                                                                           | res -> SubExpression res |> wrapit)
                match reducedTuples |> Array.forall (function | {Expr = Obj o} -> true | _ -> false) with
                | true -> reducedTuples |> (fun tc -> Obj (tupleToObj tc |> box))
                | false -> reducedTuples |> Tuple |> Unresolved
                |> wrapit |> Some           
            | Unknown unk -> bindings |> Map.tryFind unk |> Option.bind (fun res -> res.Force() |> wrapit |> Some)
            | Generator ({Expr = Obj(s)}, {Expr = Obj(i)}, {Expr = Obj(e)}) ->
                match s, i, e with
                | (:? int64 as s), (:? int64 as i), (:? int64 as e) -> Obj (seq {s .. i .. e }) |> wrapit |> Some
                | (:? float as s), (:? float as i), (:? float as e) -> Obj (seq {s .. i .. e }) |> wrapit |> Some
                | _ -> failwith (sprintf "Unexpected Generator Parameters: %A, %A, %A" s i e)
            | Generator (sexpr, iexpr, eexpr) ->
                let rs = reduceExpressions [] [sexpr] bindings
                let ri = reduceExpressions [] [iexpr] bindings
                let re = reduceExpressions [] [eexpr] bindings
                match rs, ri, re with
                | (({Expr = Obj(s)} :: []), ({Expr = Obj(i)} :: []), ({Expr = Obj(e)} :: [])) -> 
                    Generator ({sexpr with Expr = Obj(s)}, {iexpr with Expr = Obj(i)}, {eexpr with Expr = Obj(e)}) |> wrapit |> Some
                | s, i, e when not finalReduction -> 
                    let gen = {sexpr with Expr = SubExpression s}, {iexpr with Expr = SubExpression i}, {eexpr with Expr = SubExpression e}
                    Generator gen |> Unresolved |> wrapit |> Some
                | _ -> failwith (sprintf "One or more generator expressions could not be resolved: %A, %A, %A" rs ri re)
            | IfThenElse (ifexpr, thenexpr, elseexpr) ->
                match reduceExpressions [] ifexpr bindings with
                // If fully resolved in initial reduction, resolve and return the result clause
                | {Expr = Obj (:? bool as res)} :: [] -> 
                    if res then reduceExpressions [] thenexpr bindings
                    else reduceExpressions [] elseexpr bindings
                    |> (fun resExpr -> Some (SubExpression(resExpr) |> wrapit))
                // If not fully resolved in initial reduction, reduce both clauses and return the result
                // Note: If in the future globally scoped Variables are added, resolving them here will cause problems with inner non-pure calls
                | rif -> 
                    let rthen = reduceExpressions [] thenexpr bindings |> List.rev
                    let relse = reduceExpressions [] elseexpr bindings |> List.rev
                    IfThenElse (rif |> List.rev, rthen, relse) |> Unresolved |> wrapit |> Some
            // Execute Lambda when fully applied, but only on final reduction to preserve semantics
            | Lambda ({Params = []} & l) when finalReduction -> 
                let totalBindings = Seq.concat [(Map.toSeq initialBindings); (Map.toSeq l.Bindings)] |> Map.ofSeq
                SubExpression (reduceExpressions [] [l.Contents] totalBindings)  |> wrapit |> Some
            | _ -> None
            |> Option.map (fun res -> res, left, rt)
        | _ -> None

    and (|ResolveTuple|_|) bindings =
        function 
        | {Offset = lOffset; Length = lLength; Expr = l} :: lt, {Offset = rOffset; Length = rLength; Expr = r } :: rt ->
            match l, r with
            | Obj l, Postfix r -> Returned (r l) |> Some
            | Prefix l, Obj r -> Returned (l r) |> Some
            | Method l, Unit -> executeUnitMethod l
            | Method l, Obj r -> executeParameterizedMethod l r 
            | Unknown l, AppliedInvoke r when finalReduction || settings.BindGlobalsWhenReducing -> cachedResolveStatic (settings.Namespaces, l, r)
            | New, Unknown r -> Unknown r |> Some
            | Unknown l, Obj r -> executeConstructor settings.Namespaces l r
            | Invoke, Unknown r -> AppliedInvoke r |> Some
            | Invoke, IndexArgs r -> IndexArgs r |> Some // Here for F#-like indexing (if you want it)            
            | Obj l, AppliedInvoke r when finalReduction -> resolveInvoke l r
            | IndexedProperty l, IndexArgs ({ Expr = Obj r }) -> executeIndexer l r
            | Obj l, IndexArgs ({ Expr = Obj r }) -> callIndexedProperty l r
            | Lambda (lambda), Obj r -> applyArgToLambda lambda r |> Some          
            | _ -> None
            |> Option.map (fun res -> {Offset = lOffset; Length = (rOffset + rLength) - lOffset; Expr = res}, lt, rt)
        | _ -> None
    
    and (|ResolveTriple|_|) =
        function
        // Order of Operations case 1: Obj_L InfixOp_R1 Obj_R InfixOp_R2
        // If InfixOp_R1 <= InfixOp_R2 then we can safely apply InfixOp_R1
        | ((lInfix & {Expr = (Infix (lp, lfun))}) :: {Expr = Obj l} :: lt), ({Expr = Obj r} :: (rInfix & {Expr = (Infix (rp, rfrun))}) :: rt) when finalReduction ->
            if lp <= rp then 
                Some ({lInfix with Expr = Obj (lfun l r)}, lt, ({rInfix with Expr = Infix (rp, rfrun)}) :: rt)
            else None
        // Order of Operations case 2: Obj InfixOp Obj END 
        // We've reached the end of our list, it's safe to use InfixOp
        | ((lInfix & {Expr = (Infix (lp, lfun))}) :: {Expr = Obj l} :: lt), ({Expr = Obj r} :: []) when finalReduction ->
            Some ({lInfix with Expr = Obj (lfun l r)}, lt, [])
        | _ -> None

    // Tries to merge/convert local tokens, if it can't it moves to the right
    and reduceExpressions (lleft: ExprRep list) (lright: ExprRep list) (bindings: (string, ExprTypes Lazy) Map) =
        match lleft, lright with
        | left, (rExpr & {Expr = Unresolved(expr)}) :: rt -> reduceExpressions ({rExpr with Expr = expr} :: left) rt bindings
        // Binding
        | left, (({Expr = Binding (bindName, bindInnerExpr)} & bindExpr) :: rt)  ->
            match reduceExpressions [] [bindInnerExpr] bindings with
            // Recursive Lambda Binding
            | lmbExpr :: [] & {Expr = Lambda(lambda)} :: [] 
                when not finalReduction ->
                    // Bindings with the same name as lambda arguments must be removed so that names are not incorrectly bound to same-name variables in scope
                    let cleanBinds = lambda.Params |> List.fold (fun bnds pn -> if bnds |> Map.containsKey pn then bnds |> Map.remove pn else bnds) bindings         
                    let reducedExpr = reduceExpressions [] [lambda.Contents] cleanBinds
                    let recLambda = 
                        let newLambda = {lambda with Contents = { lambda.Contents with Expr = SubExpression reducedExpr }}
                        do newLambda.Bindings <- newLambda.Bindings |> Map.add bindName (Lambda newLambda |> Lazy.CreateFromValue)
                        { lmbExpr with Expr = Lambda newLambda }
                    let newbindings = bindings |> Map.add bindName (lazy recLambda.Expr)
                    reduceExpressions left rt newbindings
            // Normal Value Binding
            | rexpr -> 
                let newbindings = bindings |> Map.add bindName (lazy SubExpression rexpr) in
                    reduceExpressions left rt newbindings
        | ResolveSingle bindings (res, lt, rt)
        | ResolveTuple bindings (res, lt, rt) 
        | ResolveTriple (res, lt, rt) -> reduceExpressions lt (res :: rt) bindings
        // Continue
        | l,  r :: rt -> reduceExpressions (r :: l) rt bindings
        // Single Element, Done Reducing
        | l :: [], [] -> [l]
        | catchall when finalReduction -> failwith (sprintf "Unexpected case: %A" catchall)
        // Multiple Elements, Done Reducing
        | left, [] -> left

    reduceExpressions [] exprs initialBindings



