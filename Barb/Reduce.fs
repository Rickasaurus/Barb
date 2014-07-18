module Barb.Reduce

open System
open System.Collections
open System.Collections.Concurrent

open Barb.Interop
open Barb.Representation

type BarbExecutionException (message, trace: string, offset, length) =
    inherit BarbException (message, offset, length)
    member t.Trace = trace

let indexIntoTuple (elements: ExprTypes array) (index: obj) = 
    match index with 
    | :? int64 as idx -> elements.[int idx] 
    | _ -> failwith (sprintf "Bad type for tuple index: %A" index)

let argsFromTuple (tuple: ExprRep array) = 
    tuple |> Array.map (fun ex -> ex.Expr) 
    |> Array.map (function | Obj v -> v | other -> failwith (sprintf "Cannot resolve the given tuple-internal expression to a object type: %A" other))    

let tupleToObj (tuple: ExprRep array) = 
    tuple |> Array.map (fun ex -> ex.Expr) 
    |> Array.map (function | Obj v -> v | other -> failwith (sprintf "Cannot resolve the given tuple-internal expression to a object type: %A" other))
    |> box

let resolveExpressionResult (input: ExprRep list) =
    match input with
    | { Expr = Obj (res) } :: [] -> res
    | { Expr = Tuple (items) } :: [] -> tupleToObj items
    | otherTokens -> failwith (otherTokens |> List.fold (fun s t -> s + (sprintf "Unexpected result: %A" t.Expr)) "")

let applyArgToLambda (l: LambdaRecord) (arg: obj) =
    match l.Params with
    | [] -> failwith (sprintf "Unexpected Lambda Argument %A" arg)
    | bindname :: restprms ->
        let bindings = l.Bindings |> Map.add bindname (Existing (Obj arg |> Lazy.CreateFromValue))
        Lambda({ l with Params = restprms; Bindings = bindings })

let inline SubExpressionIfNeeded (input: ExprRep list): ExprTypes =   
    match input with
    | h :: [] -> h.Expr
    | list -> SubExpression list

let resolveExpression exprs initialBindings settings (finalReduction: bool) = 

    let rec (|ResolveSingle|_|) bindings lists =
            match lists with 
            | left, ({ Expr = rExpr; Offset = exprOffset; Length = exprLength } & erep) :: rt ->
                try 
                    let wrapit e = { erep with Expr = e }
                    match rExpr with
                    | Returned o -> resolveResultType o |> wrapit |> Some
                    | SubExpression exp ->
                        match reduceExpressions [] exp bindings |> fst |> List.rev with
                        | single :: [] -> single
                        | many -> SubExpression many |> Unresolved |> wrapit
                        |> Some
                    | IndexArgs ({ Expr = SubExpression exp }) when finalReduction ->
                        match reduceExpressions [] exp bindings |> fst |> List.rev with
                        | [] -> failwith "No indexer found in [ ]"
                        | single :: [] -> IndexArgs single |> wrapit
                        | other -> failwith "Indexer could not be fully reduced"
                        |> Some
                    | Tuple tc -> 
                        let reducedTuples = 
                            tc |> Array.map (fun t -> 
                                    match reduceExpressions [] [t] bindings |> fst  with 
                                    | res -> SubExpressionIfNeeded res |> wrapit)
                        match reducedTuples |> Array.forall (function | {Expr = Obj o} -> true | _ -> false) with
                        | true -> reducedTuples |> Tuple |> Resolved
                        | false -> reducedTuples |> Tuple |> Unresolved
                        |> wrapit |> Some           
                    | ArrayBuilder ar ->
                        let reducedArray = 
                            ar |> Array.map (fun t -> 
                                    match reduceExpressions [] [t] bindings |> fst  with 
                                    | ({ Expr = Obj o } & oobj) :: [] -> oobj 
                                    | res -> SubExpressionIfNeeded res |> wrapit)
                        match reducedArray |> Array.forall (function | {Expr = Obj o} -> true | _ -> false) with
                        | true -> reducedArray |> tupleToObj |> Obj
                        | false when reducedArray.Length = 0 -> [||] |> box |> Obj
                        | false -> reducedArray |> ArrayBuilder |> Unresolved
                        |> wrapit |> Some            
                    | Unknown unk -> 
                        match bindings |> Map.tryFind unk with
                        | Some ComingLater when finalReduction -> raise <| BarbExecutionException(sprintf "Expected value not bound: %s" unk, (sprintf "%A" lists), exprOffset, exprLength)
                        | Some ComingLater -> None
                        | Some (Existing v) -> v.Force() |> wrapit |> Some
                        | None when finalReduction -> raise <| BarbExecutionException(sprintf "Specified unknown was unable to be resolved: %s" unk, (sprintf "%A" lists), exprOffset, exprLength)
                        | None -> None
                    | AppliedProperty(o, p) -> p.GetValue(o, [||]) |> Returned |> wrapit |> Some
                    | AppliedMultiProperty(ops) -> [| for (o, pi) in ops -> pi.GetValue(o, [||]) |> Returned |> wrapit |] |> ArrayBuilder |> wrapit |> Some
                    | Generator ({Expr = Obj(s)}, {Expr = Obj(i)}, {Expr = Obj(e)}) ->
                        match s, i, e with
                        | (:? int64 as s), (:? int64 as i), (:? int64 as e) -> Obj (seq {s .. i .. e }) |> wrapit |> Some
                        | (:? float as s), (:? float as i), (:? float as e) -> Obj (seq {s .. i .. e }) |> wrapit |> Some
                        | _ -> raise <| BarbExecutionException("Unexpected Generator Paramters", (sprintf "%A, %A, %A" s i e), exprOffset, exprLength)
                    | Generator (sexpr, iexpr, eexpr) ->
                        let rs, _ = reduceExpressions [] [sexpr] bindings
                        let ri, _ = reduceExpressions [] [iexpr] bindings
                        let re, _ = reduceExpressions [] [eexpr] bindings
                        match rs, ri, re with
                        | (({Expr = Obj(s)} :: []), ({Expr = Obj(i)} :: []), ({Expr = Obj(e)} :: [])) -> 
                            Generator ({sexpr with Expr = Obj(s)}, {iexpr with Expr = Obj(i)}, {eexpr with Expr = Obj(e)}) |> wrapit |> Some
                        | s, i, e when not finalReduction -> 
                            let gen = {sexpr with Expr = SubExpressionIfNeeded s}, {iexpr with Expr = SubExpressionIfNeeded i}, {eexpr with Expr = SubExpressionIfNeeded e}
                            Generator gen |> Unresolved |> wrapit |> Some
                        | _ -> raise <| BarbExecutionException("One or more generator expressions could not be resolved", (sprintf "%A, %A, %A" rs ri re), exprOffset, exprLength)
                    | IfThenElse (ifexpr, thenexpr, elseexpr) ->
                        match reduceExpressions [] [ifexpr] bindings |> fst with
                        // If fully resolved in initial reduction, resolve and return the result clause
                        | {Expr = Obj (:? bool as res)} :: [] -> 
                            if res then reduceExpressions [] [thenexpr] bindings
                            else reduceExpressions [] [elseexpr] bindings
                            |> fun resExpr -> resExpr |> fst |> SubExpressionIfNeeded |> wrapit |> Some
                        // If not fully resolved in initial reduction, reduce both clauses and return the result
                        // Note: If in the future globally scoped Variables are added, resolving them here will cause problems with inner non-pure calls
                        | rif -> 
                            let repIf = { ifexpr with Expr = rif |> List.rev |> SubExpressionIfNeeded }
                            let repThen = { thenexpr with Expr = reduceExpressions [] [thenexpr] bindings |> fst |> List.rev |> SubExpressionIfNeeded }
                            let repElse = { elseexpr with Expr = reduceExpressions [] [elseexpr] bindings |> fst |> List.rev |> SubExpressionIfNeeded }
                            IfThenElse (repIf, repThen, repElse) |> Unresolved |> wrapit |> Some
                    // Execute Lambda when fully applied, but only on final reduction to preserve semantics
                    | Lambda ({Params = []} & l) when finalReduction -> 
                        let totalBindings = Seq.concat [(Map.toSeq initialBindings); (Map.toSeq l.Bindings)] |> Map.ofSeq
                        reduceExpressions [] [l.Contents] totalBindings |> fst |> SubExpressionIfNeeded |> wrapit |> Some
                    | And (lExpr, rExpr) ->
                        // Left and side of the And 
                        match reduceExpressions [] [lExpr] bindings |> fst with
                        // False short curcuits 
                        | { Expr = Obj (null) } :: [] -> Obj null |> wrapit |> Some
                        | { Expr = Obj (:? bool as res)} :: [] when res = false -> Obj res |> wrapit |> Some
                        | { Expr = Obj (:? bool as res)} :: [] when res = true ->
                            // Evaluate right hand side of the And
                            match reduceExpressions [] [rExpr] bindings |> fst with
                            | { Expr = Obj (null) } :: [] -> Obj null |> wrapit |> Some
                            | { Expr = Obj (:? bool as res)} :: [] -> Obj res |> wrapit |> Some
                            | res when not finalReduction -> And ({lExpr with Expr = Obj true}, {rExpr with Expr = res |> List.rev |> SubExpressionIfNeeded}) |> Unresolved |> wrapit |> Some
                            | res -> raise <| BarbExecutionException("Right hand side of And did not evaluate properly", sprintf "%A" rExpr, exprOffset, exprLength)
                        // Left hand side did not evaluate fully, try to reduce both and return
                        | res when not finalReduction -> 
                            let repL = { lExpr with Expr = res |> List.rev |> SubExpressionIfNeeded }
                            let repR = { rExpr with Expr = reduceExpressions [] [rExpr] bindings |> fst |> List.rev |> SubExpressionIfNeeded }
                            And (repL, repR) |> Unresolved |> wrapit |> Some
                        | res -> raise <| BarbExecutionException("Left hand side of And did not evaluate properly", sprintf "%A" lExpr, exprOffset, exprLength)
                    | Or (lExpr, rExpr) ->
                        // Left and side of the Or 
                        match reduceExpressions [] [lExpr] bindings |> fst with
                        // True short curcuits 
                        | { Expr = Obj (null)} :: [] -> Obj null |> wrapit |> Some
                        | { Expr = Obj (:? bool as res)} :: [] when res = true -> Obj res |> wrapit |> Some
                        | { Expr = Obj (:? bool as res)} :: [] when res = false ->
                            // Evaluate right hand side of the Or
                            match reduceExpressions [] [rExpr] bindings |> fst  with
                            | { Expr = Obj (null) } :: [] -> Obj null |> wrapit |> Some
                            | { Expr = Obj (:? bool as res)} :: [] -> Obj res |> wrapit |> Some
                            | res when not finalReduction -> 
                                let orExpr = Or ({lExpr with Expr = Obj false}, {rExpr with Expr = res |> List.rev |> SubExpressionIfNeeded}) 
                                orExpr |> Unresolved |> wrapit |> Some
                            | res -> raise <| BarbExecutionException("Right hand side of Or did not evaluate properly", sprintf "%A" rExpr, exprOffset, exprLength)
                        // Left hand side did not evaluate fully, try to reduce both and return
                        | res when not finalReduction -> 
                            let repL = { lExpr with Expr = res |> List.rev |> SubExpressionIfNeeded }
                            let repR = { rExpr with Expr = reduceExpressions [] [rExpr] bindings |> fst |> List.rev |> SubExpressionIfNeeded }
                            Or (repL, repR) |> Unresolved |> wrapit |> Some
                        | res -> raise <| BarbExecutionException("Left hand side of Or did not evaluate properly", sprintf "%A" lExpr, exprOffset, exprLength)
                    | _ -> None
                    |> Option.map (fun res -> res, left, rt)
                with
                | :? BarbException as ex -> reraise () 
                | ex -> raise <| new BarbExecutionException(ex.Message, sprintf "%A" lists, exprOffset, exprLength)
            | _ -> None

    and (|ResolveTuple|_|) bindings =
        function 
        | ({Offset = lOffset; Length = lLength; Expr = l} & lrep) :: lt, ({Offset = rOffset; Length = rLength; Expr = r } & rrep) :: rt ->
            try 
                match l, r with
                // Apply a postfix function and return the result
                | Obj l, Postfix r -> Returned (r l) |> Some
                // Apply a prefix function and return the result
                | Prefix l, Obj r -> Returned (l r) |> Some
                // Execute a parameterless method
                | InvokableExpr exp, Unit -> 
                    match exp with
                    | AppliedMethod (o,l) -> executeUnitMethod o l
                    | AppliedMultiMethod (osl) -> 
                        [| for (o,mi) in osl do yield! executeUnitMethod o mi |> Option.toArray |] // Note: Currently Drops Elements Without the given Method
                        |> Array.map (fun res -> { lrep with Expr = res })
                        |> ArrayBuilder |> Some
                // Execute some method given arguments r
                | InvokableExpr exp, ResolvedTuple r -> 
                    match exp with                                       
                    | AppliedMethod (o,l) -> executeParameterizedMethod o l r
                    | AppliedMultiMethod (osl) -> 
                        [| for (o,mi) in osl do yield! executeParameterizedMethod o mi r |> Option.toArray |] // Note: Currently Drops Elements Without the given Method
                        |> Array.map (fun res -> { lrep with Expr = res })
                        |> ArrayBuilder |> Some
                | InvokableExpr exp, Obj r -> 
                    match exp with                                       
                    | AppliedMethod (o,l) -> executeParameterizedMethod o l r
                    | AppliedMultiMethod (osl) -> 
                        [| for (o,mi) in osl do yield! executeParameterizedMethod o mi r |> Option.toArray |] // Note: Currently Drops Elements Without the given Method
                        |> Array.map (fun res -> { lrep with Expr = res })
                        |> ArrayBuilder |> Some
                // Perform a .NET-Application wide scope invocation
                | Unknown l, AppliedInvoke (depth, name) when depth = 0 && finalReduction || settings.BindGlobalsWhenReducing -> 
                    let mis = cachedResolveStatic (settings.Namespaces, l, name)
                    match mis with
                    | mi :: [] -> mi |> Some
                    | [] -> None
                    | mis -> failwith "Multiple results returned for invoke"
                // Perform a .NET-Application wide scope invocation
                | Unknown l, AppliedInvoke (depth, r) when depth > 0 && finalReduction || settings.BindGlobalsWhenReducing -> 
                    raise <| new BarbExecutionException (sprintf "Static invocations above depth 0 are not currently supported.", sprintf "%A" (l,r), lOffset, (rOffset - lOffset) + rLength)
                // New is allowed so C# users feel at home, it really does nothing though
                | New, Unknown r -> Unknown r |> Some
                // Provides F#-like construction without new
                | Unknown l, Obj r -> executeConstructor settings.Namespaces l r
                // Provides F#-like construction without new
                | Unknown l, ResolvedTuple r -> executeConstructor settings.Namespaces l r
                // Simplify to a single invocation ExprType
                | Invoke, Unknown r -> AppliedInvoke (0, r) |> Some
                // Here for F#-like indexing, the invoking '.' is simply removed 
                | Invoke, IndexArgs r -> IndexArgs r |> Some
                // Invoke on null should always be null
                | Obj null, AppliedInvoke _ -> Obj null |> Some
                // Finds and returns memebers of the given name of the given object
                | Obj l, AppliedInvoke (0, r) when finalReduction -> 
                    try resolveInvokeByInstance l r
                    with ex -> raise <| BarbExecutionException(ex.Message, sprintf "%A" (l,r), rOffset, rLength)
                | Obj l, AppliedInvoke (depth, r) when finalReduction -> 
                    try 
                        let res = resolveInvokeAtDepth depth l r 
                        match res |> List.collect (fun (o, rms) -> rms) with
                        | EmptyResolution -> None 
                        | MixedResolution _ -> failwithf "Both properties and methods were found for %s in the nested invocation." r
                        | AllResolvedMethod _ -> 
                            let mires = res |> List.map (fun (o, rms) -> o, rms |> List.map (function | ResolvedMethod mi -> mi))
                            AppliedMultiMethod(mires) |> InvokableExpr |> Some
                        | AllResolvedProperty _ -> 
                            let pires = res |> List.map (fun (o, rms) -> o, rms |> (function | [ResolvedProperty mi] -> mi))
                            AppliedMultiProperty(pires) |> Some
                    with ex -> raise <| BarbExecutionException(ex.Message, sprintf "%A" (l,r), rOffset, rLength)
                // Combine Invocations for Flattened Nested Invocation
                | Invoke, AppliedInvoke (depth, r) -> AppliedInvoke (depth + 1, r) |> Some
                // Index the given indexed property representation via IndexArgs
                | AppliedIndexedProperty (o,l), IndexArgs ({ Expr = Obj r }) -> executeIndexer o l r
                // Index the given object it via IndexArgs
                | Obj l, IndexArgs ({ Expr = Obj r }) -> callIndexedProperty l r
                // Partially apply the given object to the lambda.  Will execute the a lambda if it's the final argument.
                | Lambda (lambda), Obj r -> applyArgToLambda lambda r |> Some          
                | _ -> None
                // Maps the text representation of the two input expressions to the single result expression
                |> Option.map (fun res -> {Offset = lOffset; Length = (rOffset + rLength) - lOffset; Expr = res}, lt, rt)
            with
            | :? BarbException as ex -> reraise ()  
            | ex ->
                let totalLength = (rOffset - lOffset) + rLength 
                raise <| new BarbExecutionException (sprintf "Unexpected exception in tuple reduction: %s" ex.Message, sprintf "%A" (l,r), lOffset, totalLength)
        | _ -> None
    
    and (|ResolveTriple|_|) =
            function
            // Order of Operations case 1: Obj_L InfixOp_R1 Obj_R InfixOp_R2
            // If InfixOp_R1 <= InfixOp_R2 then we can safely apply InfixOp_R1
            | ((lInfix & {Offset = oOffset; Length = oLength; Expr = (Infix (lp, lfun))}) :: {Expr = Obj l} :: lt), 
              ({Expr = Obj r} :: (rInfix & {Expr = (Infix (rp, rfrun))}) :: rt) when finalReduction ->
                    if lp <= rp then Some ({lInfix with Expr = Obj (lfun l r)}, lt, ({rInfix with Expr = Infix (rp, rfrun)}) :: rt)
                    else None
            // Order of Operations case 2: Obj InfixOp Obj END 
            // We've reached the end of our list, it's safe to use InfixOp
            | ((lInfix & {Offset = oOffset; Length = oLength; Expr = (Infix (lp, lfun))}) :: {Expr = Obj l} :: lt), ({Expr = Obj r} :: []) when finalReduction ->
                    Some ({lInfix with Expr = Obj (lfun l r)}, lt, [])
            | _ -> None

    // Tries to merge/convert local tokens, if it can't it moves to the right
    and reduceExpressions (lleft: ExprRep list) (lright: ExprRep list) (bindings: Bindings) : ExprRep list * Bindings=
        match lleft, lright with
        | left, (rExpr & {Expr = Unresolved(expr)}) :: rt -> 
            reduceExpressions ({rExpr with Expr = expr} :: left) rt bindings
        // Binding
        | left, (({Expr = BVar (bindName, bindInnerExpr, boundScope)} & bindExpr) :: rt)  ->
            match reduceExpressions [] [bindInnerExpr] bindings |> fst with
            // Recursive Lambda Binding
            | lmbExpr :: [] & {Expr = Lambda(lambda)} :: [] 
                when not finalReduction ->
                    // Bindings with the same name as lambda arguments must be removed so that names are not incorrectly bound to same-name variables in scope
                    let cleanBinds = lambda.Params |> List.fold (fun bnds pn -> if bnds |> Map.containsKey pn then bnds |> Map.remove pn else bnds) bindings         
                    let reducedExpr, _ = reduceExpressions [] [lambda.Contents] cleanBinds
                    let recLambda = 
                        let newLambda = {lambda with Contents = { lambda.Contents with Expr = SubExpressionIfNeeded reducedExpr }}
                        do newLambda.Bindings <- newLambda.Bindings |> Map.add bindName (Existing (Lambda newLambda |> Lazy.CreateFromValue))
                        { lmbExpr with Expr = Lambda newLambda }
                    let newbindings = bindings |> Map.add bindName (Existing (lazy recLambda.Expr))
                    let res = { bindExpr with Expr = reduceExpressions [] [boundScope] newbindings |> fst |> SubExpressionIfNeeded }
                    reduceExpressions left (res :: rt) newbindings
            // Normal Value Binding
            | rexpr -> 
                let newbindings = bindings |> Map.add bindName (Existing (lazy SubExpressionIfNeeded rexpr)) in
                    let res = { bindExpr with Expr = reduceExpressions [] [boundScope] newbindings |> fst |> SubExpressionIfNeeded }
                    reduceExpressions left (res :: rt) bindings
        | ResolveSingle bindings (res, lt, rt)
        | ResolveTuple bindings (res, lt, rt) 
        | ResolveTriple (res, lt, rt) -> reduceExpressions lt (res :: rt) bindings
        // Continue
        | l,  r :: rt -> reduceExpressions (r :: l) rt bindings
        // Single Element, Done Reducing
        | l :: [], [] -> [l], bindings
        | ((lh :: lr) & left, (rh :: rr) & right) when finalReduction -> 
            let offset, length = 
                match (left, right) with
                | {Offset = lOffset; Length = lLength} :: _, {Offset = rOffset; Length = rLength} :: _ -> lOffset, (rOffset - lOffset) + rLength 
                | {Offset = lOffset; Length = lLength} :: _, _ -> lOffset, lLength
                | _,  {Offset = rOffset; Length = rLength} :: _ -> rOffset, rLength
                | _ -> 0u, 0u
            let message = sprintf "Unexpected case: %A %A" lh rh
            let trace = sprintf "%A" (left, right)
            raise <| new BarbExecutionException (message, trace, offset, length)
        // Multiple Elements, Done Reducing
        | left, [] -> left, bindings

    reduceExpressions [] exprs initialBindings



