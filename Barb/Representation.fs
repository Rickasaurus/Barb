module Barb.Representation

open System
open System.Collections.Generic

type BarbSettings = 
    {
        BindGlobalsWhenReducing: bool
        Namespaces: string Set
        AdditionalBindings: IDictionary<string,obj>
    }
    with static member Default = 
        { 
            BindGlobalsWhenReducing = true
            AdditionalBindings = [] |> dict
            Namespaces = [null; ""; "System"] |> Set.ofList
        }

type MethodSig = ((obj array -> obj) * Type array) list

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
    // Lambda: Parameters, Args, Contents
    | Lambda of string list * ExprTypes list * ExprTypes
    | IfThenElse of ExprTypes list * ExprTypes list * ExprTypes list
    | Generator of ExprTypes * ExprTypes * ExprTypes
    // Has no Unknowns
    | Resolved of ExprTypes
    // Has Unknowns
    | Unresolved of ExprTypes

type BarbData = 
    {
        InputType: Type
        OutputType: Type
        Contents: ExprTypes list
        Settings: BarbSettings
    }
    with static member Default = { InputType = typeof<unit>; OutputType = typeof<unit>; Contents = []; Settings = BarbSettings.Default }