module Barb.Representation

open System

type MethodSig = ((obj array -> obj) * Type array) list

type MemberTypes = 
    | PropertyCall of (obj -> obj)
    | IndexedPropertyCall of (obj -> ((obj array -> obj) * Type array) list)
    | MethodCall of (obj -> ((obj array -> obj) * Type array) list)

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
    | AppliedInvoke of string
    | Unknown of string
    | Binding of string * ExprTypes
    | Lambda of string list * ExprTypes list * ExprTypes
    | IfThenElse of ExprTypes list * ExprTypes list * ExprTypes list
    | Generator of ExprTypes * ExprTypes * ExprTypes
    | Fold of ExprTypes * ExprTypes
    | Resolved of ExprTypes