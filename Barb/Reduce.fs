module Barb.Reduce

open System
open System.Collections
open System.Collections.Concurrent

open Barb.Interop
open Barb.Representation

let rec applyInstanceState (input: obj) exprs =
    let rec resolveInstanceType expr =
            match expr with 
            | ParentProperty (call) -> Returned (call input)
            | SubExpression (subEx) -> SubExpression (applyInstanceState input subEx)
            | Tuple (tuple) -> Tuple (applyInstanceState input tuple) 
            | IndexArgs (argEx) -> IndexArgs (resolveInstanceType argEx)
            | other -> other
    exprs |> List.map (fun expr -> resolveInstanceType expr)
    
let resolveExpression exprs (failOnUnresolved: bool) = 
    let rec (|ResolveSingle|_|) =
        function 
        | Returned o -> Some <| resolveResultType o
        | Tuple tc -> 
            let resolvedTp = tc |> List.collect (fun t -> reduceExpressions [] [t]) |> List.rev |> ResolvedTuple
            Some resolvedTp
        | Unknown unk -> Some <| Obj unk
        | _ -> None

    and attemptToResolvePair =
        function
        | ObjToObj l, Obj r -> Some <| Obj (l r)
        | Obj l, (Infix (ObjToObjToBool r)) -> Some <| ObjToBool (r l)
        | Bool l, (Infix (ObjToObjToBool r)) -> Some <| ObjToBool (r l)
        | Bool l, (Infix (BoolToBoolToBool r)) -> Some <| BoolToBool (r l)
        | ObjToBool l, Obj r -> Some <| Bool (l r)
        | ObjToBool l, Bool r -> Some <| Bool (l r)
        | BoolToBool l, Bool r -> Some <| Bool (l r)
        | Method l, Unit -> executeUnitMethod l
        | Method l, Obj r -> executeOneParamMethod l r
        | Method l, ResolvedTuple r -> executeParameterizedMethod l r 
        | Invoke, Unknown r -> Some <| AppliedInvoke r
        | Invoke, ResolvedIndexArgs r -> Some <| ResolvedIndexArgs r //Here for F#-like indexing (if you want it)
        | Obj l, AppliedInvoke r -> resolveInvoke l r
        | IndexedProperty l, ResolvedIndexArgs (Obj r) -> executeOneParamMethod l r
        | Obj l, ResolvedIndexArgs (Obj r) -> callIndexedProperty l r
        | _ -> None

    and reduceExpressions lleft lright =
        match lleft, lright with
        | (ResolveSingle resolved :: lt), right -> reduceExpressions lt (resolved :: right)
        | left, (SubExpression exp :: rt) ->
            match reduceExpressions [] exp |> List.rev with
            | single :: [] -> reduceExpressions left (single :: rt)
            | many -> reduceExpressions (SubExpression many :: left) rt
        | left, (IndexArgs exp :: rt) ->
            match reduceExpressions [] [exp] with
            | [] -> failwith (sprintf "No indexer found in [ ]")
            | single :: [] -> reduceExpressions left (ResolvedIndexArgs single :: rt)
            | other -> failwith (sprintf "Multi-indexing not currently supported")
        | [], r :: rt -> reduceExpressions [r] rt
        | l :: [], [] -> [l]
        | l :: lt, r :: rt ->        
            match attemptToResolvePair (l, r) with
            | Some (rToken) -> reduceExpressions lt (rToken :: rt)
            | None -> reduceExpressions (r :: l :: lt) rt
        | catchall when failOnUnresolved -> failwith (sprintf "Unexpected case: %A" catchall)
        | left, [] -> left

    reduceExpressions [] exprs



