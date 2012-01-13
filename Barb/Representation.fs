module Barb.Representation

open System
open System.Collections.Generic

type BarbSettings = 
    {
        OptimizeForImmutability: bool
        AdditionalBindings: IDictionary<string,obj>
    }
    with static member Default = { OptimizeForImmutability = true; AdditionalBindings = [] |> dict }

type MethodSig = ((obj array -> obj) * Type array) list

//type TupleStruct =
//   struct
//      val Items: obj array
//   end

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
    | Tuple of ExprTypes array
    | IndexArgs of ExprTypes
    | AppliedInvoke of string
    | Unknown of string
    | Binding of string * ExprTypes
    | Lambda of string list * ExprTypes list * ExprTypes
    | IfThenElse of ExprTypes list * ExprTypes list * ExprTypes list
    | Generator of ExprTypes * ExprTypes * ExprTypes
    // Has no Unknowns
    | Resolved of ExprTypes
    // Has Unknowns
    | Unresolved of ExprTypes