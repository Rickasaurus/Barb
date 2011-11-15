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

//let rec applyInstanceState (input: obj) exprs =
//    let rec resolveInstanceType expr =
//            match expr with 
////            | ParentProperty (call) -> Returned (call input)
//            | SubExpression (subEx) -> SubExpression (applyInstanceState input subEx)
//            | Tuple (tuple) -> Tuple (applyInstanceState input tuple) 
//            | IndexArgs (argEx) -> IndexArgs (resolveInstanceType argEx)
//            | other -> other
//    exprs |> List.map (fun expr -> resolveInstanceType expr)

let attemptToResolvePair =        
    function
    | Prefix l, Obj r -> Obj (l r) |> Some
    | Method l, Unit -> executeUnitMethod l
    | Method l, Obj r -> executeParameterizedMethod l r 
    | Invoke, Unknown r -> Some <| AppliedInvoke r
    | Invoke, ResolvedIndexArgs r -> Some <| ResolvedIndexArgs r //Here for F#-like indexing (if you want it)
    | Obj l, AppliedInvoke r -> resolveInvoke l r
    | IndexedProperty l, ResolvedIndexArgs (Obj r) -> executeIndexer l r
    | Obj l, ResolvedIndexArgs (Obj r) -> callIndexedProperty l r
    | LambdaPartial l, Obj r -> Some <| (l (Obj r))
    | _ -> None
    
let resolveExpression exprs initialBindings (finalReduction: bool) = 
    let rec buildLambdaFunction (arguments: ExprTypes list) (body: ExprTypes list) =
        let args = arguments |> List.map (function | Unknown str -> str | other -> failwith (sprintf "Unexpected lambda argument type: %A" other))
        let inputs = ref []
        let rec consumeArg arg = 
            inputs := (arg :: !inputs)
            if List.length !inputs = List.length arguments then
                let bindings = List.zip args (List.rev !inputs) |> Map.ofList
                SubExpression (reduceExpressions [] body bindings)
            else LambdaPartial consumeArg
        LambdaPartial consumeArg

    and (|ResolveIfThenElse|_|) bindings =
        function
        | IfThenElse (ifexpr, thenexpr, elseexpr) ->
            match reduceExpressions [] ifexpr bindings with
            | Obj (:? bool as res) :: [] when finalReduction -> 
                if res then reduceExpressions [] thenexpr bindings
                else reduceExpressions [] elseexpr bindings
                |> (fun resExpr -> Some (SubExpression(resExpr)))
            | invalid when finalReduction -> failwith (sprintf "If predicate returned an invalid result: %A" invalid)
            | _ -> None
        | _ -> None
            
    and (|ResolveSingle|_|) bindings =
        function 
        | Returned o -> Some <| resolveResultType o
        | Tuple tc -> 
            let resolvedTp = tc |> List.collect (fun t -> reduceExpressions [] [t] bindings) |> List.rev |> tupleToSequence
            Some (Obj resolvedTp)
        | Unknown unk -> bindings |> Map.tryFind unk
        | ResolveIfThenElse bindings result -> Some result
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
    and reduceExpressions lleft lright bindings =
        #if DEBUG
        printfn "L: %A R: %A" lleft lright
        #endif
        match lleft, lright with
        | (LambdaDef (names, expr) :: lt), right -> let lambdaExpr = reduceExpressions [] [expr] bindings
                                                    let lambdaResult = buildLambdaFunction names lambdaExpr
                                                    reduceExpressions lt (lambdaResult :: right) bindings 
        | (Binding (name, expr) :: lt), right -> let newbindings = bindings |> Map.add name expr in reduceExpressions lt right newbindings
        | (ResolveSingle bindings resolved :: lt), right -> reduceExpressions lt (resolved :: right) bindings
        | left, (SubExpression exp :: rt) ->
            match reduceExpressions [] exp bindings |> List.rev with
            | single :: [] -> reduceExpressions left (single :: rt) bindings
            | many -> reduceExpressions (SubExpression many :: left) rt bindings
        | left, (IndexArgs exp :: rt) ->
            match reduceExpressions [] [exp] bindings with
            | [] -> failwith (sprintf "No indexer found in [ ]")
            | single :: [] -> reduceExpressions left (ResolvedIndexArgs single :: rt) bindings
            | other -> failwith (sprintf "Multi-indexing not currently supported")
        | [], r :: rt -> reduceExpressions [r] rt bindings
        | l :: [], [] -> [l]
        | ResolveTriple (res, lt, rt) -> reduceExpressions lt (res :: rt) bindings
        | l :: lt, r :: rt ->        
            match attemptToResolvePair (l, r) with
            | Some (rToken) -> reduceExpressions lt (rToken :: rt) bindings
            | None -> reduceExpressions (r :: l :: lt) rt bindings
        | catchall when finalReduction -> failwith (sprintf "Unexpected case: %A" catchall)
        | left, [] -> left

    reduceExpressions [] exprs initialBindings



