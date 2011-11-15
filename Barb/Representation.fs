module Barb.Representation

open System

type MethodSig = ((obj array -> obj) * Type array) list

type MemberTypes = 
    | PropertyCall of (obj -> obj)
    | IndexedPropertyCall of (obj -> ((obj array -> obj) * Type array) list)
    | MethodCall of (obj -> ((obj array -> obj) * Type array) list)

type ExprTypes = 
    | ParentProperty of (obj -> obj)
    | Method of MethodSig
    | IndexedProperty of MethodSig
//    | BoolToBoolToBool of (bool -> bool -> bool)
//    | BoolToBool of (bool -> bool)
//    | Bool of bool
//    | ObjToObjToBool of (obj -> obj -> bool)
//    | ObjToBool of (obj -> bool)
    | Obj of obj
    | Returned of obj
//    | Infix of int * ExprTypes
    | Prefix of (obj -> obj)    
    | Postfix of (obj -> obj)
    | Infix of int * (obj -> obj -> obj) 
    | Unit
    | SubExpression of ExprTypes list
    | Tuple of ExprTypes list
//    | ResolvedTuple of ExprTypes list
    | IndexArgs of ExprTypes
    | ResolvedIndexArgs of ExprTypes
    | Invoke
    | AppliedInvoke of string
    | Unknown of string
    | Binding of string * ExprTypes
    | LambdaDef of ExprTypes list * ExprTypes
    | LambdaPartial of (ExprTypes -> ExprTypes)

