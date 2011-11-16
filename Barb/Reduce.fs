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
            | Unknown v -> yield v :> obj
            | what -> failwith (sprintf "Cannot resolve the given tuple-internal expression to a object type: %A" what)
    }




let resolveExpression exprs initialBindings (finalReduction: bool) = 

    let rec (|ResolveIfThenElse|_|) bindings =
        function
        | Resolved (IfThenElse (ifexpr, thenexpr, elseexpr)) when finalReduction ->
            match reduceExpressions [] ifexpr bindings with
            | Obj (:? bool as res) :: [] -> 
                if res then reduceExpressions [] thenexpr bindings
                else reduceExpressions [] elseexpr bindings
                |> (fun resExpr -> Some (SubExpression(resExpr)))
            | invalid -> failwith (sprintf "If predicate returned an invalid result: %A" invalid)
        | IfThenElse (ifexpr, thenexpr, elseexpr) ->
            match reduceExpressions [] ifexpr bindings with
            | Obj (:? bool as res) :: [] -> 
                if res then reduceExpressions [] thenexpr bindings
                else reduceExpressions [] elseexpr bindings
                |> (fun resExpr -> Some (SubExpression(resExpr)))
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
        | Returned o -> resolveResultType o |> Some
        | Resolved (Tuple tc) when finalReduction ->
            tc |> List.collect (fun t -> reduceExpressions [] [t] bindings) |> tupleToSequence |> box |> Obj |> Some
        | Tuple tc -> 
            tc |> List.collect (fun t -> reduceExpressions [] [t] bindings) |> List.rev |> Tuple |> Resolved |> Some
        | Unknown unk -> bindings |> Map.tryFind unk |> Option.bind (fun res -> Some <| res.Force())
        | ResolveIfThenElse bindings result -> Some result
        | _ -> None

    and attemptToResolvePair bindings =
        function
        | Obj l, Postfix r -> Obj (r l) |> Some
        | Prefix l, Obj r -> Obj (l r) |> Some
        | Method l, Unit -> executeUnitMethod l
        | Method l, Obj r -> executeParameterizedMethod l r 
        | Invoke, Unknown r -> AppliedInvoke r |> Some
        | Invoke, Resolved (IndexArgs r) -> IndexArgs r |> Resolved |> Some //Here for F#-like indexing (if you want it)
        | Obj l, AppliedInvoke r -> resolveInvoke l r
        | IndexedProperty l, Resolved (IndexArgs (Obj r)) -> executeIndexer l r
        | Obj l, Resolved (IndexArgs (Obj r)) -> callIndexedProperty l r
        | Lambda (p,a,e), Obj r -> applyArgToLambda bindings (p,a,e) r |> Some
        | _ -> None    
    
    and (|ResolveTriple|_|) =
        function
        // Obj InfixOp Obj InfixOp
        | (Infix (lp, lfun)) :: Obj l :: lt, Obj r :: (Infix (rp, rfrun)) :: rt when finalReduction ->
            if lp <= rp then 
                Some (Obj (lfun l r), lt, (Infix (rp, rfrun)) :: rt)
            else None
        // Obj InfixOp Obj END
        | (Infix (lp, lfun)) :: Obj l :: lt, Obj r :: [] when finalReduction ->
            Some (Obj (lfun l r), lt, [])
        | _ -> None

    // Always looks on the left and moves to the right first, then tries to merge left and right
    and reduceExpressions lleft lright (bindings: (string, ExprTypes Lazy) Map) =
        #if DEBUG
        printfn "L: %A R: %A" lleft lright
        #endif
        match lleft, lright with
        | (Binding (name, expr) :: lt), right -> let newbindings = bindings |> Map.add name (lazy (expr)) in reduceExpressions lt right newbindings
        | (ResolveSingle bindings resolved :: lt), right -> reduceExpressions lt (resolved :: right) bindings
        | left, (SubExpression exp :: rt) ->
            match reduceExpressions [] exp bindings |> List.rev with
            | single :: [] -> reduceExpressions left (single :: rt) bindings
            | many -> reduceExpressions (SubExpression many :: left) rt bindings
        | left, (IndexArgs exp :: rt) ->
            match reduceExpressions [] [exp] bindings with
            | [] -> failwith (sprintf "No indexer found in [ ]")
            | single :: [] -> reduceExpressions left (Resolved (IndexArgs single) :: rt) bindings
            | other -> failwith (sprintf "Multi-indexing not currently supported")
        | [], r :: rt -> reduceExpressions [r] rt bindings
        | l :: [], [] -> [l]
        | ResolveTriple (res, lt, rt) -> reduceExpressions lt (res :: rt) bindings
        | l :: lt, r :: rt ->        
            match attemptToResolvePair bindings (l, r) with
            | Some (rToken) -> reduceExpressions lt (rToken :: rt) bindings
            | None -> reduceExpressions (r :: l :: lt) rt bindings
        | catchall when finalReduction -> failwith (sprintf "Unexpected case: %A" catchall)
        | left, [] -> left

    reduceExpressions [] exprs initialBindings



