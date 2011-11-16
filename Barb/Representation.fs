module Barb.Representation

open System

type MethodSig = ((obj array -> obj) * Type array) list

type MemberTypes = 
    | PropertyCall of (obj -> obj)
    | IndexedPropertyCall of (obj -> ((obj array -> obj) * Type array) list)
    | MethodCall of (obj -> ((obj array -> obj) * Type array) list)

//[<CustomEquality>]
//[<NoComparison>]
type ExprTypes = 
    | Unit
    | Invoke
    | Method of MethodSig
    | IndexedProperty of MethodSig
    | Obj of obj
    | Returned of obj
    | Prefix of (obj -> obj)    
    | Postfix of (obj -> obj)
    | Infix of int * (obj -> obj -> obj) 
    | SubExpression of ExprTypes list
    | Tuple of ExprTypes list
    | IndexArgs of ExprTypes
//    | ResolvedIndexArgs of ExprTypes
    | AppliedInvoke of string
    | Unknown of string
    | Binding of string * ExprTypes
//    | LambdaDef of ExprTypes list * ExprTypes
//    | LambdaPartial of (ExprTypes -> ExprTypes)
    | Lambda of string list * ExprTypes list * ExprTypes
    | IfThenElse of ExprTypes list * ExprTypes list * ExprTypes list
    | Resolved of ExprTypes
//    with
//        override t.Equals(yobj) =
//            match yobj with
//            | :? ExprTypes as y ->
//                match t, y with
//                | Unit, Unit
//                | Invoke, Invoke -> true
//                | Method l, Method r   
//                | IndexedProperty l, IndexedProperty r -> Object.Equals (l, r)
//                | Returned l, Returned r  
//                | Obj l, Obj r -> Object.Equals (l, r)
//                | Prefix l, Prefix r 
//                | Postfix l, Postfix r -> Object.Equals (l, r)
//                | Infix (lp, lf), Infix (rp, rf) -> lp = rp && Object.Equals (lf, rf)
//                | SubExpression ll, SubExpression rl
//                | Tuple ll, Tuple rl -> List.forall2 (fun l r -> l.Equals r) ll rl 
//                | IndexArgs l, IndexArgs r
//                | ResolvedIndexArgs l, ResolvedIndexArgs r -> l.Equals r
//                | AppliedInvoke l, AppliedInvoke r 
//                | Unknown l, Unknown r -> l = r
//                | Binding (ls, le), Binding (rs, re) -> ls = rs && le.Equals(re)
////                | LambdaDef (lp, le), LambdaDef (rp, re) -> List.forall2 (fun l r -> l.Equals r) lp rp && le.Equals(re)                     
////                | LambdaPartial l, LambdaPartial r -> Object.Equals(l, r)
//                | Lambda (ln, lv, le), Lambda (rn, rv, re) -> ln = rn && List.forall2 (fun l r -> l.Equals r) lv rv && le.Equals(re)
//                | IfThenElse (li, lt, le), IfThenElse (ri, rt, re) -> List.forall2 (fun l r -> l.Equals r) li ri && List.forall2 (fun l r -> l.Equals r) lt rt && List.forall2 (fun l r -> l.Equals r) le re
//                | _ -> false
//            | _ -> false
