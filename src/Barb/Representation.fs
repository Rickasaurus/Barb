/// Internal representation bits.
module Barb.Representation

open System
open System.Collections.Generic
open System.Reflection

/// The base Barb exception type
type BarbException (message, offset: uint32, length: uint32) = 
    inherit Exception (message)
    /// Offset of the substring of the Barb input string at which the error occured.
    member t.Offset = offset
    /// Length of the substring of the Barb input string at which the error occured.
    member t.Length = length

/// For Barb exceptions that happen after the parse stage
type BarbExecutionException (message, trace: string, offset, length) =
    inherit BarbException (message, offset, length)
    /// The reduction being processed when the error occured.
    member t.Trace = trace
    override t.ToString() = message + Environment.NewLine + trace

/// The settings used by Barb in execution
type BarbSettings = 
    {
        /// Turning this off will cause Barb to not pull in globals in the optimization stage. 
        BindGlobalsWhenReducing: bool
        /// This is for debugging purposes only and turning it off may cause infinite loops. 
        FailOnCatchAll: bool
        /// Namespaces that will be open to Barb functions
        Namespaces: string Set
        /// Additional variable bindings for Barb
        AdditionalBindings: IDictionary<string,obj>
    }
    with static member Default = 
            { 
                BindGlobalsWhenReducing = true
                FailOnCatchAll = true
                AdditionalBindings = [] |> dict
                Namespaces = [null; ""; "System"; "Microsoft.FSharp"; "Microsoft.FSharp.Collections"; "Barb.Lib"; "Barb.Lib.TopLevel"] |> Set.ofList
            }

type MethodSig = ((obj array -> obj) * Type array) list

/// Internal representation of a Barb lambda expression (Mutable so we can update the bindings with itself for easy recursion.)
type LambdaRecord = { Params: string list; mutable Bindings: Bindings; Contents: ExprRep }

/// The internal Barb potential method call representations
and InvokableExpr =
    | AppliedMultiMethod of (obj * MethodInfo list) list
    | AppliedMethod of obj * MethodInfo list    

/// The internal Barb expression tree. Public for debugging purposes.
and ExprTypes = 
    (* Units *)
    | Unit
    | Invoke
    | New
    | InvokableExpr of InvokableExpr
    | AppliedProperty of obj * PropertyInfo
    | AppliedMultiProperty of (obj * PropertyInfo) list
    | AppliedInvoke of int * string // where int is the collection depth to perform the invocation, 0 is top level    
    | AppliedIndexedProperty of obj * PropertyInfo list
    | FieldGet of FieldInfo list
    | Obj of obj
    | Prefix of (obj -> obj)    
    | Postfix of (obj -> obj)
    | Infix of int * (obj -> obj -> obj) 
    | IndexArgs of ExprRep array
    | Unknown of string
    (* Multi-Subexpression Containers *)
    | SubExpression of ExprRep list
    | Tuple of ExprRep array
    | ArrayBuilder of ExprRep array
    | SetBuilder of ExprRep array
    // Bound Value: Name, Bound Expression, Scope
    | BVar of string * ExprRep * ExprRep
    | Lambda of LambdaRecord
    | IfThenElse of ExprRep * ExprRep * ExprRep
    | Generator of ExprRep * ExprRep * ExprRep
    | And of ExprRep * ExprRep
    | Or of ExprRep * ExprRep
    (* Tags *)
    // Returned by a .NET call of some kind
    | Returned of obj
    // Has no Unknowns
    | Resolved of ExprTypes
    // Has Unknowns
    | Unresolved of ExprTypes
    with override t.ToString () = sprintf "(%A)" t

/// The internal Barb type used for tracking where each subexpression came from in the original input string.
and ExprRep =
    {
        Offset: uint32
        Length: uint32
        Expr: ExprTypes
    }
    with override t.ToString() = sprintf "{ Off = %i; Len = %i; %A }" t.Offset t.Length t.Expr

/// Representation of potential variable bindings in Barb
and BindingContents = 
    | ComingLater
    /// Offset -> Length -> ExprRep
    | Existing of (uint32 -> uint32 -> ExprRep)

and Bindings = (String, BindingContents) Map 

/// The internal datatype used for the compilation/interpretation pipeline. 
type BarbData = 
    {
        InputType: Type 
        OutputType: Type
        Contents: ExprRep list
        Settings: BarbSettings
    }
    with static member Default = { InputType = typeof<unit>; OutputType = typeof<unit>; Contents = []; Settings = BarbSettings.Default }

let internal exprRepListOffsetLength (exprs: ExprRep seq) =
    let offsets = exprs |> Seq.map (fun e -> e.Offset)
    let max = offsets |> Seq.max 
    let min = offsets |> Seq.min
    min, max - min

let internal listToSubExpression (exprs: ExprRep list) =
    let offset, length = exprRepListOffsetLength exprs
    { Offset = offset; Length = length; Expr = SubExpression exprs }

let rec internal exprExistsInRep (pred: ExprTypes -> bool)  (rep: ExprRep) =
    exprExists pred rep.Expr
and internal exprExists (pred: ExprTypes -> bool) (expr: ExprTypes) =
    match expr with
    | _ when pred expr -> true 
    | SubExpression (repList) -> repList |> List.exists (exprExistsInRep pred)
    | Tuple (repArray) -> repArray |> Array.exists (exprExistsInRep pred)
    | IndexArgs (repArray) -> repArray |> Array.exists (exprExistsInRep pred)
    | BVar (name, rep, scopeRep) -> 
        exprExistsInRep pred rep || exprExistsInRep pred scopeRep
    | Lambda (lambda) -> exprExistsInRep pred (lambda.Contents)
    | IfThenElse (ifRep, thenRep, elseRep) ->
        ifRep |> exprExistsInRep pred || thenRep |> exprExistsInRep pred || elseRep |> exprExistsInRep pred
    | Generator (fromRep, incRep, toRep) -> [fromRep; incRep; toRep] |> List.exists (exprExistsInRep pred) 
    // The two tagged cases
    | Resolved (rep) -> exprExists pred rep
    | Unresolved (expr) -> exprExists pred expr
    // Nothing found
    | _ -> false

let internal wrapResolved (rep: ExprRep) = { rep with Expr = Resolved rep.Expr }
let internal wrapUnresolved (rep: ExprRep) = { rep with Expr = Unresolved rep.Expr }

let internal (|ResolvedTuple|_|) (v: ExprTypes) =
    match v with 
    | Resolved(Tuple tc) -> 
        tc |> Array.map (fun ex -> ex.Expr) 
        |> Array.map (function | Obj v -> v | other -> failwith (sprintf "Resolved tuple should only contian objects: %A" other))
        |> Some
    | _ -> None

let internal (|ResolvedIndexArgs|_|) (v: ExprTypes) =
    match v with 
    | Resolved(IndexArgs tc) -> 
        tc |> Array.map (fun ex -> ex.Expr) 
        |> Array.map (function | Obj v -> v | other -> failwith (sprintf "Resolved index args should only contian objects: %A" other))
        |> Some
    | _ -> None


let internal wrapExistingBinding expr = (fun off len -> {Offset = off; Length = len; Expr = expr}) |> Existing