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
                Namespaces = [null; ""; "System"; "Microsoft.FSharp"; "Microsoft.FSharp.Collections"; "Barb.Lib"] |> Set.ofList
            }

type MethodSig = ((obj array -> obj) * Type array) list

type ExprTypes = 
    | Unit
    | Invoke
    | New
    | Method of MethodSig
    | IndexedProperty of MethodSig
    | Obj of obj
    | Returned of obj
    | Prefix of (obj -> obj)    
    | Postfix of (obj -> obj)
    | Infix of int * (obj -> obj -> obj) 
    | SubExpression of ExprRep list
    | Tuple of ExprRep array
    | IndexArgs of ExprRep
    | AppliedInvoke of string
    | Unknown of string
    | Binding of string * ExprRep
    // Lambda: Parameters, Args, Contents
    | Lambda of string list * ExprTypes list * ExprRep
    | IfThenElse of ExprRep list * ExprRep list * ExprRep list
    | Generator of ExprRep * ExprRep * ExprRep
    // Has no Unknowns
    | Resolved of ExprRep
    // Has Unknowns
    | Unresolved of ExprTypes

and ExprRep =
    {
        Offset: int
        Length: int
        Expr: ExprTypes
    }

type BarbData = 
    {
        InputType: Type
        OutputType: Type
        Contents: ExprRep list
        Settings: BarbSettings
    }
    with static member Default = { InputType = typeof<unit>; OutputType = typeof<unit>; Contents = []; Settings = BarbSettings.Default }

let exprRepListOffsetLength (exprs: ExprRep seq) =
    let offsets = exprs |> Seq.map (fun e -> e.Offset)
    let max = offsets |> Seq.max 
    let min = offsets |> Seq.min
    min, max - min

let listToSubExpression (exprs: ExprRep list) =
    let offset, length = exprRepListOffsetLength exprs
    { Offset = offset; Length = length; Expr = SubExpression exprs }