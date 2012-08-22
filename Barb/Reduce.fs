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

let resolveExpression exprs initialBindings settings (finalReduction: bool) = 

    let rec applyArgToLambda bindings (prms, eargs: ExprTypes list, expr) (arg: obj) =
        let args = Obj arg :: eargs
        if List.length prms = List.length args then
            let newbindings = 
                args 
                |> List.map Lazy.CreateFromValue 
                |> List.rev
                |> List.zip prms 
                |> Seq.append (Map.toSeq bindings) 
                |> Map.ofSeq 
            SubExpression (reduceExpressions [] [expr] newbindings)
        else
            Lambda(prms, args, expr)

    and (|ResolveSingle|_|) bindings =
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
            | Lambda (p,a,e), Obj r -> applyArgToLambda bindings (p,a,e) r |> Some            
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
            | lmbExpr :: [] & {Expr = Lambda(p,a,lexpr)} :: [] 
                when not finalReduction
                && lexpr |> exprExistsInRep (function | Unknown name when name = bindName -> true | _ -> false) ->
                    let cleanBinds = p |> List.fold (fun bnds pn -> if bnds |> Map.containsKey pn then bnds |> Map.remove pn else bnds) bindings         
                    let reducedExpr = reduceExpressions [] [lexpr] cleanBinds
                    let recLambda = 
//                        let reducedLambda = {lmbExpr with Expr = Lambda(p,a,{lexpr with Expr = SubExpression reducedExpr})} in
//                        let newBind = {bindExpr with Expr = Binding(bindName, reducedLambda)} in
//                        let newSubExpr = {lmbExpr with Expr = SubExpression (newBind::reducedExpr)} in
//                        let newLambdaExpr = {lmbExpr with Expr = Lambda(p,a,newSubExpr) }
//                        { bindExpr with Expr = Binding(bindName, newLambdaExpr) }
                        { bindExpr with 
                            Expr = Binding(bindName, { lmbExpr with Expr = SubExpression ({bindExpr with Expr = Binding(bindName, {lmbExpr with Expr = Lambda(p,a,{ lmbExpr with Expr = SubExpression reducedExpr })})} :: reducedExpr)})
                        }
                    reduceExpressions left (recLambda :: rt) bindings
            // Non-Recursive Lambda Binding
            | lmbExpr :: [] & {Expr = Lambda(p,a,lexpr)} :: []  when not finalReduction ->
                let reducedLambda = Lambda(p,a, { lexpr with Expr = SubExpression <| reduceExpressions [] [lexpr] bindings })
                let newbindings = bindings |> Map.add bindName (lazy reducedLambda) in
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



