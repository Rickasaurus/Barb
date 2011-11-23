module Barb.Reduce

open System
open System.Collections
open System.Collections.Concurrent

open Barb.Interop
open Barb.Representation

let resolveExpressionResult (input: ExprTypes list) =
    match input with
    | Obj (res) :: [] -> res
    | otherTokens -> failwith (sprintf "Unexpected result: %A" otherTokens)


let tupleToSequence (tuple: ExprTypes list) = 
    seq {
        for t in tuple do
            match t with
            | Obj v -> yield box v
            | what -> failwith (sprintf "Cannot resolve the given tuple-internal expression to a object type: %A" what)
    }

let resolveExpression exprs initialBindings (finalReduction: bool) = 

    let rec (|ResolveIfThenElse|_|) bindings =
        function        
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
                IfThenElse (rif |> List.rev, rthen, relse) |> Resolved |> Some
        | _ -> None

    and applyArgToLambda bindings lambda (arg: obj) =
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
            | Tuple tc when finalReduction ->
                tc |> List.collect (fun t -> reduceExpressions [] [t] bindings) |> List.rev |> tupleToSequence |> box |> Obj |> Some
            | Tuple tc -> 
                tc |> List.collect (fun t -> reduceExpressions [] [t] bindings) |> Tuple |> Resolved |> Some
            | Unknown unk -> bindings |> Map.tryFind unk |> Option.bind (fun res -> Some <| res.Force())
            | ResolveIfThenElse bindings result -> Some result
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
                | s, i, e when not finalReduction -> Generator (SubExpression s, SubExpression i, SubExpression e) |> Resolved |> Some
                | _ -> failwith (sprintf "One or more generator expressions are unresolved: %A, %A, %A" rs ri re)
                //
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
            | Invoke, Unknown r -> AppliedInvoke r |> Some
            | Invoke, IndexArgs r -> IndexArgs r |> Some // Here for F#-like indexing (if you want it)
            | Obj l, AppliedInvoke r -> resolveInvoke l r
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
        #if DEBUG
        printfn "L: %A" lleft 
        printfn "R: %A" lright
        printfn "B: %A" bindings
        printfn ""
        #endif
        match lleft, lright with
        | left, Resolved(expr) :: rt -> reduceExpressions (expr :: left) rt bindings
        | left, (Binding (name, expr) :: rt) ->
            match reduceExpressions [] [expr] bindings with
            | Lambda(p,a,lexpr) :: [] when not finalReduction -> 
                let recLambda = Binding(name, Lambda(p,a,SubExpression [Binding(name, Lambda(p,a,lexpr)); lexpr])) |> Resolved
                reduceExpressions left (recLambda :: rt) bindings
            | rexpr -> let newbindings = bindings |> Map.add name (lazy SubExpression rexpr) in  reduceExpressions left rt newbindings
        | left, (SubExpression exp :: rt) ->
            match reduceExpressions [] exp bindings |> List.rev with
            | single :: [] -> reduceExpressions left (single :: rt) bindings
            | many -> reduceExpressions (SubExpression many :: left) rt bindings         
        | left, (IndexArgs (SubExpression exp) :: rt) when finalReduction ->
            match reduceExpressions [] exp bindings |> List.rev with
            | [] -> failwith (sprintf "No indexer found in [ ]")
            | single :: [] -> reduceExpressions left ((IndexArgs single) :: rt) bindings
            | other -> failwith (sprintf "Indexer could not be fully reduced.")
        | ResolveTriple (res, lt, rt) -> reduceExpressions lt (res :: rt) bindings
        | ResolveTuple bindings (res, lt, rt) -> reduceExpressions lt (res :: rt) bindings   
        | ResolveSingle bindings (res, lt, rt) -> reduceExpressions lt (res :: rt) bindings
        | l,  r :: rt -> reduceExpressions (r :: l) rt bindings
        | l :: [], [] -> [l]
        | catchall when finalReduction -> failwith (sprintf "Unexpected case: %A" catchall)
        | left, [] -> left

    reduceExpressions [] exprs initialBindings



