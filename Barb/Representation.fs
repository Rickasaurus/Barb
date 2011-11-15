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
    | Obj of obj
    | Returned of obj
    | Prefix of (obj -> obj)    
    | Postfix of (obj -> obj)
    | Infix of int * (obj -> obj -> obj) 
    | Unit
    | SubExpression of ExprTypes list
    | Tuple of ExprTypes list
    | IndexArgs of ExprTypes
    | ResolvedIndexArgs of ExprTypes
    | Invoke
    | AppliedInvoke of string
    | Unknown of string
    | Binding of string * ExprTypes
    | LambdaDef of ExprTypes list * ExprTypes
    | LambdaPartial of (ExprTypes -> ExprTypes)
    | IfThenElse of ExprTypes list * ExprTypes list * ExprTypes list

