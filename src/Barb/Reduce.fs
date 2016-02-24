module internal Barb.Reduce

open System
open System.Collections
open System.Collections.Concurrent
open System.Reflection

open Barb.Interop
open Barb.Representation
open Barb.Helpers

let indexIntoTuple (elements: ExprTypes array) (index: obj) = 
    match index with 
    | :? int64 as idx -> elements.[int idx] 
    | _ -> failwith (sprintf "Bad type for tuple index: %A" index)

let exprRepsToObjs (tuple: ExprRep array) = 
    tuple |> Array.map (fun ex -> ex.Expr) 
    |> Array.map (function | Obj v -> v | other -> failwith (sprintf "Cannot resolve the given tuple-internal expression to a object type: %A" other))    

let resolveExpressionResult (input: ExprRep list) =
    match input with
    | { Expr = Obj (res) } :: [] -> res
    | { Expr = Tuple (items) } :: [] -> exprRepsToObjs items |> box
    | otherTokens -> failwith (otherTokens |> List.fold (fun s t -> s + (sprintf "Unexpected result: %A" t.Expr)) "")

let applyArgToLambda (l: LambdaRecord) (arg: obj) =
    match l.Params with
    | [] -> failwith (sprintf "Unexpected Lambda Argument %A" arg)
    | bindname :: restprms ->
        let bindings = l.Bindings |> Map.add bindname (wrapExistingBinding (Obj arg))
        Lambda({ l with Params = restprms; Bindings = bindings })

let inline SubExpressionIfNeeded (input: ExprRep list): ExprTypes =   
    match input with
    | h :: [] -> h.Expr
    | list -> SubExpression list

let resolveToObjs reducer wrapit resultCtor reps = 
    let reducedReps = 
        reps |> Array.map (fun t -> 
                match reducer t with 
                | res -> SubExpressionIfNeeded res |> wrapit)
    match reducedReps |> Array.forall (function | {Expr = Obj o} -> true | _ -> false) with
    | true -> reducedReps |> resultCtor |> Resolved
    | false -> reducedReps |> resultCtor |> Unresolved
    |> wrapit |> Some         

let resolveArrayBuilder arrayReducer wrapit builderReps = 
    // True when all contents are Obj or reducedArray is empty                      
    let allObj = ref true 
    let arrTyp = ref None
    let arrTypSame = ref true
    let reducedArray = 
        builderReps
        |> Array.map (fun t -> 
                    match arrayReducer t with
                    | ({ Expr = Obj o } & oobj) :: [] -> 
                        let oTyp = o.GetType()
                        if !arrTypSame then
                            match !arrTyp with
                            | None -> arrTyp := Some oTyp
                            | Some aTyp when aTyp = oTyp -> ()
                            | Some aTyp -> arrTyp := None; arrTypSame := false
                        oobj 
                    | res -> allObj := false; SubExpressionIfNeeded res |> wrapit)                         
    match !allObj, reducedArray |> Array.isEmpty, !arrTyp with
    | true, false, Some typ -> 
        // Create Typed Array
        let objArr = reducedArray |> exprRepsToObjs
        let newArr = Array.CreateInstance(typ, objArr.Length)
        do Array.Copy(objArr, newArr, objArr.Length) 
        newArr |> box |> Obj
    | true, false, None -> 
        // Create Untyped Array
        reducedArray |> exprRepsToObjs |> box |> Obj
    | false, false, _ -> 
        // Created unresolved expression
        reducedArray |> ArrayBuilder |> Unresolved
    | _ -> [||] |> box |> Obj
    |> wrapit |> Some   

let resolveExpression exprs initialBindings settings (finalReduction: bool) = 

    let rec (|ResolveSingle|_|) bindings lists =
            let inline reduceSubexpr subExpr = reduceExpressions [] [subExpr] bindings |> fst
            let inline reduceSubexprCont subExpr f = f <| (reduceExpressions [] [subExpr] bindings |> fst)
            let inline reduceExpressionsCont (lleft: ExprRep list) (lright: ExprRep list) (bindings: Bindings) f = 
                reduceExpressions lleft lright bindings |> fst |> f
            let inline reduceSubexprsCont subExpr f = reduceExpressions [] subExpr bindings |> fst |> List.rev |> f 
            match lists with 
            | left, ({ Expr = rExpr; Offset = exprOffset; Length = exprLength } & erep) :: rt ->
//                try 
                    let wrapErep e = { erep with Expr = e }
                    let wrapit e = { erep with Expr = e }, left, rt
                    let wrapRep e = e, left, rt
                    match rExpr with
                    | Returned o -> Cont { return resolveResultType o |> wrapit |> Some } |> Some
                    | SubExpression exp ->
                        Cont {
                            let! redExpr = reduceSubexprsCont exp
                            return match redExpr with
                                   | single :: [] -> single |> wrapRep |> Some
                                   | many -> SubExpression many |> Unresolved |> wrapit  |> Some 
                        } |> Some
                    | IndexArgs tc -> 
                            // Need to make this a proper continuation 
                            Cont { return tc |> resolveToObjs (fun t -> reduceSubexpr t) wrapErep IndexArgs |> Option.map wrapRep } |> Some
                    | Tuple tc -> 
                            // Need to make this a proper continuation 
                            Cont { return tc |> resolveToObjs (fun t -> reduceSubexpr t) wrapErep Tuple |> Option.map wrapRep } |> Some
                    | ArrayBuilder ar -> 
                            // Need to make this a proper continuation
                            Cont { return ar |> resolveArrayBuilder (fun t -> reduceSubexpr t) wrapErep |> Option.map wrapRep } |> Some
                    | Unknown unk -> 
                        Cont {
                            return match bindings |> Map.tryFind unk with
                                    | Some ComingLater when finalReduction -> raise <| BarbExecutionException(sprintf "Expected value not bound: %s" unk, (sprintf "%A" lists), exprOffset, exprLength)
                                    | Some ComingLater -> None 
                                    | Some (Existing v) -> v exprOffset exprLength |> wrapRep |> Some 
                                    | None when finalReduction -> raise <| BarbExecutionException(sprintf "Specified unknown was unable to be resolved: %s" unk, (sprintf "%A" lists), exprOffset, exprLength)
                                    | None -> None 
                        } |> Some
                    | AppliedProperty(o, p) -> Cont { return p.GetValue(o, [||]) |> Returned |> wrapit |> Some } |> Some
                    | AppliedMultiProperty(ops) -> 
                        Cont {
                            return [| for (o, pi) in ops do 
                                        let v = pi.GetValue(o, [||])
                                        if v <> null then yield v |> Returned |> wrapErep 
                                        |] |> ArrayBuilder |> wrapit  |> Some
                        } |> Some
                    | Generator ({Expr = Obj(s)}, {Expr = Obj(i)}, {Expr = Obj(e)}) ->
                        Cont {
                            return match s, i, e with
                                   | (:? int64 as s), (:? int64 as i), (:? int64 as e) -> Obj (seq {s .. i .. e }) |> wrapit |> Some
                                   | (:? float as s), (:? float as i), (:? float as e) -> Obj (seq {s .. i .. e }) |> wrapit |> Some
                                   | _ -> raise <| BarbExecutionException("Unexpected Generator Paramters", (sprintf "%A, %A, %A" s i e), exprOffset, exprLength)
                        } |> Some
                    | Generator (sexpr, iexpr, eexpr) -> 
                        Cont {
                            let! redSExpr = reduceSubexprCont sexpr
                            let! redIExpr = reduceSubexprCont iexpr
                            let! redEexpr = reduceSubexprCont eexpr
                            match redSExpr, redIExpr, redEexpr with
                            | (({Expr = Obj(s)} :: []), ({Expr = Obj(i)} :: []), ({Expr = Obj(e)} :: [])) -> 
                                return Generator ({sexpr with Expr = Obj(s)}, {iexpr with Expr = Obj(i)}, {eexpr with Expr = Obj(e)}) |> wrapit |> Some
                            | s, i, e when not finalReduction -> 
                                let gen = {sexpr with Expr = SubExpressionIfNeeded s}, {iexpr with Expr = SubExpressionIfNeeded i}, {eexpr with Expr = SubExpressionIfNeeded e}
                                return Generator gen |> Unresolved |> wrapit |> Some
                            | rs, ri, re -> return raise <| BarbExecutionException("One or more generator expressions could not be resolved", (sprintf "%A, %A, %A" rs ri re), exprOffset, exprLength)
                        } |> Some
                    | IfThenElse (ifexpr, thenexpr, elseexpr) ->
                        Cont {
                            let! rIfexpr = reduceSubexprCont ifexpr
                            match rIfexpr with
                            // If fully resolved in initial reduction, resolve and return the result clause
                            | {Expr = Obj (:? bool as res)} :: [] -> 
                                let branchExpr = if res then thenexpr else elseexpr
                                let! rBranchExpr = reduceSubexprCont branchExpr
                                return rBranchExpr |> SubExpressionIfNeeded |> wrapit |> Some
                            // If not fully resolved in initial reduction, reduce both clauses and return the result
                            // Note: If in the future globally scoped Variables are added, resolving them here will cause problems with inner non-pure calls
                            | rif -> 
                                let repIf = { ifexpr with Expr = rif |> List.rev |> SubExpressionIfNeeded }
                                let! rThenExpr = reduceSubexprCont thenexpr
                                let repThen = { thenexpr with Expr = rThenExpr |> List.rev |> SubExpressionIfNeeded }
                                let! rElseExpr = reduceSubexprCont elseexpr
                                let repElse = { elseexpr with Expr = rElseExpr |> List.rev |> SubExpressionIfNeeded }
                                return IfThenElse (repIf, repThen, repElse) |> Unresolved |> wrapit |> Some
                        } |> Some
                    // Execute Lambda when fully applied but abort if it doesn't fully reduce
                    | Lambda {Params = []; Contents = lambdaContents; Bindings = lambdaBindings } -> 
                        Cont {
                            let totalBindings = Seq.concat [(Map.toSeq initialBindings); (Map.toSeq lambdaBindings)] |> Map.ofSeq
                            let! redLambda = reduceExpressionsCont [] [lambdaContents] totalBindings
                            match redLambda with
                            | {Expr = SubExpression (v :: [])} :: [] -> return Some (wrapRep v)
                            | {Expr = SubExpression _ } :: [] -> return None
                            | result :: [] -> return Some (wrapRep result) 
                            | many -> return None
                        } |> Some
                    | And (lExpr, rExpr) ->
                        Cont {
                            // Left and side of the And 
                            let! redLExpr = reduceSubexprCont lExpr 
                            match redLExpr with
                            // False short curcuits 
                            | { Expr = Obj (null) } :: [] -> return Obj null |> wrapit |> Some
                            | { Expr = Obj (:? bool as res)} :: [] when res = false -> return Obj res |> wrapit |> Some
                            | { Expr = Obj (:? bool as res)} :: [] when res = true ->
                                // Evaluate right hand side of the And
                                let! redRExpr = reduceSubexprCont rExpr
                                match redRExpr with
                                | { Expr = Obj (null) } :: [] -> return Obj null |> wrapit |> Some
                                | { Expr = Obj (:? bool as res)} :: [] -> return Obj res |> wrapit |> Some
                                | res when not finalReduction -> return And ({lExpr with Expr = Obj true}, {rExpr with Expr = res |> List.rev |> SubExpressionIfNeeded}) |> Unresolved |> wrapit |> Some
                                | res -> return raise <| BarbExecutionException("Right hand side of And did not evaluate properly", sprintf "%A" rExpr, exprOffset, exprLength)
                            // Left hand side did not evaluate fully, try to reduce both and return
                            | res when not finalReduction -> 
                                let repL = { lExpr with Expr = res |> List.rev |> SubExpressionIfNeeded }
                                let! redRExpr = reduceSubexprCont rExpr
                                let repR = { rExpr with Expr = redRExpr |> List.rev |> SubExpressionIfNeeded }
                                return And (repL, repR) |> Unresolved |> wrapit |> Some
                            | res -> return raise <| BarbExecutionException("Left hand side of And did not evaluate properly", sprintf "%A" lExpr, exprOffset, exprLength)
                        } |> Some
                    | Or (lExpr, rExpr) ->
                        Cont {
                            // Left and side of the Or 
                            let! redLExpr = reduceSubexprCont lExpr
                            match redLExpr with
                            // True short curcuits 
                            | { Expr = Obj (null)} :: [] -> return Obj null |> wrapit |> Some
                            | { Expr = Obj (:? bool as res)} :: [] when res = true -> return Obj res |> wrapit |> Some
                            | { Expr = Obj (:? bool as res)} :: [] when res = false ->                               
                                // Evaluate right hand side of the Or
                                let! redRExpr = reduceSubexprCont rExpr
                                match redRExpr with
                                | { Expr = Obj (null) } :: [] -> return Obj null |> wrapit |> Some
                                | { Expr = Obj (:? bool as res)} :: [] -> return Obj res |> wrapit |> Some
                                | res when not finalReduction -> 
                                    let orExpr = Or ({lExpr with Expr = Obj false}, {rExpr with Expr = res |> List.rev |> SubExpressionIfNeeded}) 
                                    return orExpr |> Unresolved |> wrapit |> Some
                                | res -> return raise <| BarbExecutionException("Right hand side of Or did not evaluate properly", sprintf "%A" rExpr, exprOffset, exprLength)
                            // Left hand side did not evaluate fully, try to reduce both and return
                            | res when not finalReduction -> 
                                let repL = { lExpr with Expr = res |> List.rev |> SubExpressionIfNeeded }
                                let! redRExpr = reduceSubexprCont rExpr
                                let repR = { rExpr with Expr = redRExpr |> List.rev |> SubExpressionIfNeeded }
                                return Or (repL, repR) |> Unresolved |> wrapit |> Some
                            | res -> return raise <| BarbExecutionException("Left hand side of Or did not evaluate properly", sprintf "%A" lExpr, exprOffset, exprLength)
                        } |> Some
                    | _ -> None
//                with
//                | :? BarbException as ex -> reraise () 
//                | ex -> raise <| new BarbExecutionException(ex.Message, sprintf "%A" lists, exprOffset, exprLength)
            | _ ->  None 

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
                    | AppliedMethod (o,l) -> 
                        try executeUnitMethod o l |> Returned |> Some
                        with | (:? TargetInvocationException as ex) -> 
                                raise <| BarbExecutionException("Exception occured while invoking a unit method: " + ex.InnerException.Message, sprintf "%A" (l,r), rOffset, rLength)
                    | AppliedMultiMethod (osl) ->
                        try 
                            [| for (o,mi) in osl do yield executeUnitMethod o mi |]
                            |> Array.map (fun res -> { lrep with Expr = res |> Returned })
                            |> ArrayBuilder |> Some
                        with | (:? TargetInvocationException as ex) -> 
                                raise <| BarbExecutionException("Exception occured while multi invoking a unit method: " + ex.InnerException.Message, sprintf "%A" (l,r), rOffset, rLength)
                // Execute some method given arguments 
                | InvokableExpr exp, ResolvedTuple r -> 
                    match exp with                                       
                    | AppliedMethod (o,l) -> 
                        try executeParameterizedMethod o l r |> Returned |> Some
                        with | (:? TargetInvocationException as ex) -> raise <| BarbExecutionException("Exception occured while invoking a method: " + ex.InnerException.Message, sprintf "%A" (l,r), rOffset, rLength)
                    | AppliedMultiMethod (osl) -> 
                        try
                            [| for (o,mi) in osl do yield executeParameterizedMethod o mi r |> Returned |] 
                            |> Array.map (fun res -> { lrep with Expr = res })
                            |> ArrayBuilder |> Some
                        with | (:? TargetInvocationException as ex) -> 
                                raise <| BarbExecutionException("Exception occured while multi invoking a method: " + ex.InnerException.Message, sprintf "%A" (l,r), rOffset, rLength)
                | InvokableExpr exp, Obj r -> 
                    match exp with                                       
                    | AppliedMethod (o,l) -> 
                        try executeParameterizedMethod o l [|r|] |> Returned |> Some
                        with | (:? TargetInvocationException as ex) -> 
                                    raise <| BarbExecutionException("Exception occured while invoking a method on an external object: " + ex.InnerException.Message, sprintf "%A" (l,r), rOffset, rLength)
                    | AppliedMultiMethod (osl) -> 
                        try
                            [| for (o,mi) in osl do yield executeParameterizedMethod o mi [|r|] |> Returned |] 
                            |> Array.map (fun res -> { lrep with Expr = res })
                            |> ArrayBuilder |> Some
                        with | (:? TargetInvocationException as ex) -> 
                                raise <| BarbExecutionException("Exception occured while multi invoking a method on an external object: " + ex.InnerException.Message, sprintf "%A" (l,r), rOffset, rLength)                       
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
                | Unknown l, Obj r -> executeConstructor settings.Namespaces l [|r|]
                // Provides F#-like construction without new
                | Unknown l, ResolvedTuple r -> executeConstructor settings.Namespaces l r
                // Simplify to a single invocation ExprType
                | Invoke, Unknown r -> AppliedInvoke (0, r) |> Some
                // Here for F#-like indexing, the invoking '.' is simply removed 
                | Invoke, IndexArgs r -> IndexArgs r |> Some
                | Invoke, (ResolvedIndexArgs r) & rhs -> rhs |> Some
                // Invoke on null should always be null
                | Obj null, AppliedInvoke _ -> Obj null |> Some
                // Nested Invoke on an empty array should always be empty
                | Obj (:? Array as arr), AppliedInvoke (n,_) when n > 0 && arr.Length = 0 -> Obj (Array.empty<obj>) |> Some
                // Finds and returns memebers of the given name of the given object
                | Obj l, AppliedInvoke (0, r) -> 
                    try resolveInvokeByInstance l r
                    with ex -> raise <| BarbExecutionException(ex.Message, sprintf "%A" (l,r), rOffset, rLength)
                | Obj l, AppliedInvoke (depth, r) -> 
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
                | AppliedIndexedProperty (o,l), ResolvedIndexArgs args -> executeIndexer o l args
                // Index the given object it via IndexArgs
                | Obj l, ResolvedIndexArgs args -> callIndexedProperty l args
                // Partially apply the given object to the lambda.  Will execute the a lambda if it's the final argument.
                | Lambda (lambda), Obj r -> applyArgToLambda lambda r |> Some          
                | _ -> None
                // Maps the text representation of the two input expressions to the single result expression
                |> Option.map (fun res -> {Offset = lOffset; Length = (rOffset + rLength) - lOffset; Expr = res}, lt, rt)
            with
            | :? BarbException as ex -> reraise ()  
            | ex ->
                let totalLength = (rOffset - lOffset) + rLength 
                raise <| new BarbExecutionException (sprintf "Unexpected exception in tuple reduction: %s" ex.Message, sprintf "(%s, %s)" (l.ToString()) (r.ToString()), lOffset, totalLength)
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

    and reduceExpressions (lleft: ExprRep list) (lright: ExprRep list) (bindings: Bindings) : ExprRep list * Bindings =
       reduceExpressionsCont lleft lright bindings id

    // Tries to merge/convert local tokens, if it can't it moves to the right
    and reduceExpressionsCont (lleft: ExprRep list) (lright: ExprRep list) (bindings: Bindings) cont : ExprRep list * Bindings =
        match lleft, lright with
        // Eliminate any single case subexpression
        | left, (rExpr & {Expr = SubExpression(expr :: [])}) :: rt -> Cont { let! res = reduceExpressionsCont left (expr :: rt) bindings
                                                                             return res } 
        | (lExpr & {Expr = SubExpression(expr::[])}) :: lt, right -> Cont { let! res = reduceExpressionsCont (expr :: lt) right bindings
                                                                            return res } 
        // Move unresolved expressions to the left hand list
        | left, (rExpr & {Expr = Unresolved(expr)}) :: rt -> 
            Cont { let! res = reduceExpressionsCont ({rExpr with Expr = expr} :: left) rt bindings 
                   return res }
        // Binding
        | left, (({Expr = BVar (bindName, bindInnerExpr, boundScope)} & bindExpr) :: rt)  ->
            Cont {
                let! (resInnerExpr, _) = reduceExpressionsCont [] [bindInnerExpr] bindings
                match resInnerExpr with
                // Recursive Lambda Binding
                | lmbExpr :: [] & {Expr = Lambda(lambda)} :: [] when not finalReduction ->
                        // Bindings with the same name as lambda arguments must be removed so that names are not incorrectly bound to same-name variables in scope
                        let cleanBinds = lambda.Params |> List.fold (fun bnds pn -> if bnds |> Map.containsKey pn then bnds |> Map.remove pn else bnds) bindings 
                        let! (reducedExpr, _) = reduceExpressionsCont [] [lambda.Contents] cleanBinds
                        let recLambda = 
                            let newLambda = {lambda with Contents = { lambda.Contents with Expr = SubExpressionIfNeeded reducedExpr }}   
                            do newLambda.Bindings <- newLambda.Bindings |> Map.add bindName (newLambda |> Lambda |> wrapExistingBinding)
                            { lmbExpr with Expr = Lambda newLambda }
                        let newbindings = bindings |> Map.add bindName (wrapExistingBinding recLambda.Expr)
                        let! (reduced, binds) = reduceExpressionsCont left (boundScope :: rt) newbindings
                        return reduced, binds
                // Normal Value Binding
                | rexpr ->
                        let newbindings = bindings |> Map.add bindName (wrapExistingBinding (SubExpressionIfNeeded rexpr)) 
                        let! (resExpr, _) = reduceExpressionsCont [] [boundScope] newbindings
                        let res = { bindExpr with Expr = resExpr |> SubExpressionIfNeeded }
                        return! reduceExpressionsCont left (res :: rt) bindings
            } 

        | ResolveTuple bindings (res, lt, rt) 
        | ResolveTriple (res, lt, rt) -> Cont { let! res = reduceExpressionsCont lt (res :: rt) bindings
                                                return res } 
        | (ResolveSingle bindings fx) & (l,  r :: rt) -> 
            Cont { 
               let! mb = fx 
               match mb with
               | Some (res, lt, rrt) -> 
                   let! fres = reduceExpressionsCont lt (res :: rrt) bindings
                   return fres
               | None -> 
                  let! res = reduceExpressionsCont (r :: l) rt bindings 
                  return res 
            }
        // Continue
        | l,  r :: rt -> Cont { let! res = reduceExpressionsCont (r :: l) rt bindings 
                                return res } 
        // Single Element, Done Reducing
        | l :: [], [] -> Cont { return [l], bindings }
        // Unexpected case, fail with  error message
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
        | left, [] -> Cont { return left, bindings }
        <| cont

    reduceExpressions [] exprs initialBindings



