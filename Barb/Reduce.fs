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

let tupleToObj (tuple: ExprTypes array) = 
    tuple |> Array.map (function | Obj v -> v | other -> failwith (sprintf "Cannot resolve the given tuple-internal expression to a object type: %A" other))

let resolveExpressionResult (input: ExprTypes list) =
    match input with
    | Obj (res) :: [] -> res
    | Tuple (items) :: [] -> tupleToObj items |> box
    | otherTokens -> failwith (sprintf "Unexpected result: %A" otherTokens)

let resolveExpression exprs initialBindings settings (finalReduction: bool) = 

    let rec applyArgToLambda bindings lambda (arg: obj) =
        let prms, eargs, expr = lambda
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
        | left, r :: rt ->
            match r with
            | Returned o -> resolveResultType o |> Some
            | SubExpression exp ->
                match reduceExpressions [] exp bindings |> List.rev with
                | single :: [] -> single
                | many -> SubExpression many |> Unresolved  
                |> Some
            | IndexArgs (SubExpression exp) when finalReduction ->
                match reduceExpressions [] exp bindings |> List.rev with
                | [] -> failwith (sprintf "No indexer found in [ ]")
                | single :: [] -> IndexArgs single
                | other -> failwith (sprintf "Indexer could not be fully reduced.")
                |> Some
            | Tuple tc -> 
                let reducedTuples = tc |> Array.map (fun t -> match reduceExpressions [] [t] bindings with | Obj o :: [] -> Obj o | res -> SubExpression res)
                match reducedTuples |> Array.forall (function | Obj o -> true | _ -> false) with
                | true -> reducedTuples |> (fun tc -> Obj (tupleToObj tc |> box))
                | false -> reducedTuples |> Tuple |> Unresolved
                |> Some           
            | Unknown unk -> bindings |> Map.tryFind unk |> Option.bind (fun res -> Some <| res.Force())
            | Generator (Obj(s), Obj(i), Obj(e)) ->
                match s, i, e with
                | (:? int64 as s), (:? int64 as i), (:? int64 as e) -> Obj (seq {s .. i .. e }) |> Some
                | (:? float as s), (:? float as i), (:? float as e) -> Obj (seq {s .. i .. e }) |> Some
                | _ -> failwith (sprintf "Unexpected Generator Parameters: %A, %A, %A" s i e)
            | Generator (sexpr, iexpr, eexpr) ->
                let rs = reduceExpressions [] [sexpr] bindings
                let ri = reduceExpressions [] [iexpr] bindings
                let re = reduceExpressions [] [eexpr] bindings
                match rs, ri, re with
                | (Obj(s) :: []), (Obj(i) :: []), (Obj(e) :: []) -> Generator (Obj s, Obj i, Obj e) |> Some
                | s, i, e when not finalReduction -> Generator (SubExpression s, SubExpression i, SubExpression e) |> Unresolved |> Some
                | _ -> failwith (sprintf "One or more generator expressions could not be resolved: %A, %A, %A" rs ri re)
            | IfThenElse (ifexpr, thenexpr, elseexpr) ->
                match reduceExpressions [] ifexpr bindings with
                // If fully resolved in initial reduction, resolve and return the result clause
                | Obj (:? bool as res) :: [] -> 
                    if res then reduceExpressions [] thenexpr bindings
                    else reduceExpressions [] elseexpr bindings
                    |> (fun resExpr -> Some (SubExpression(resExpr)))
                // If not fully resolved in initial reduction, reduce both clauses and return the result
                // Note: If in the future globally scoped Variables are added, resolving them here will cause problems with inner non-pure calls
                | rif -> 
                    let rthen = reduceExpressions [] thenexpr bindings |> List.rev
                    let relse = reduceExpressions [] elseexpr bindings |> List.rev
                    IfThenElse (rif |> List.rev, rthen, relse) |> Unresolved |> Some
            | _ -> None
            |> Option.map (fun res -> res, left, rt)
        | _ -> None

    and (|ResolveTuple|_|) bindings =
        function 
        | l :: lt, r :: rt ->
            match l, r with
            | Obj l, Postfix r -> Obj (r l) |> Some
            | Prefix l, Obj r -> Obj (l r) |> Some
            | Method l, Unit -> executeUnitMethod l
            | Method l, Obj r -> executeParameterizedMethod l r 
            | Unknown l, AppliedInvoke r when finalReduction || settings.BindGlobalsWhenReducing -> cachedResolveStatic (settings.Namespaces, l, r)
            | Invoke, Unknown r -> AppliedInvoke r |> Some
            | Invoke, IndexArgs r -> IndexArgs r |> Some // Here for F#-like indexing (if you want it)            
            | Obj l, AppliedInvoke r when finalReduction -> resolveInvoke l r
            | IndexedProperty l, IndexArgs (Obj r) -> executeIndexer l r
            | Obj l, IndexArgs (Obj r) -> callIndexedProperty l r
            | Lambda (p,a,e), Obj r -> applyArgToLambda bindings (p,a,e) r |> Some            
            | _ -> None
            |> Option.map (fun res -> res, lt, rt)
        | _ -> None
    
    and (|ResolveTriple|_|) =
        function
        // Order of Operations case 1: Obj_L InfixOp_R1 Obj_R InfixOp_R2
        // If InfixOp_R1 <= InfixOp_R2 then we can safely apply InfixOp_R1
        | (Infix (lp, lfun)) :: Obj l :: lt, Obj r :: (Infix (rp, rfrun)) :: rt when finalReduction ->
            if lp <= rp then Some (Obj (lfun l r), lt, (Infix (rp, rfrun)) :: rt)
            else None
        // Order of Operations case 2: Obj InfixOp Obj END 
        // We've reached the end of our list, it's safe to use InfixOp
        | (Infix (lp, lfun)) :: Obj l :: lt, Obj r :: [] when finalReduction ->
            Some (Obj (lfun l r), lt, [])
        | _ -> None

    // Tries to merge/convert local tokens, if it can't it moves to the right
    and reduceExpressions lleft lright (bindings: (string, ExprTypes Lazy) Map) =
        match lleft, lright with
        | left, Unresolved(expr) :: rt -> reduceExpressions (expr :: left) rt bindings
        | left, (Binding (name, expr) :: rt)  ->
            match reduceExpressions [] [expr] bindings with
            // Lambda Binding, may be recursive
            | Lambda(p,a,lexpr) :: [] when not finalReduction -> 
                let cleanBinds = p |> List.fold (fun bnds pn -> if bnds |> Map.containsKey pn then bnds |> Map.remove pn else bnds) bindings         
                let reducedExpr = reduceExpressions [] [lexpr] cleanBinds |> SubExpression
                let recLambda = Binding(name, Lambda(p,a,SubExpression [Binding(name, Lambda(p,a,reducedExpr)); reducedExpr])) |> Unresolved
                reduceExpressions left (recLambda :: rt) bindings
            // Normal Value Binding
            | rexpr -> 
                let newbindings = bindings |> Map.add name (lazy SubExpression rexpr) in
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



