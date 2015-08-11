namespace Barb

// TODO:
// Add "global world" bindings which can be optimzied 

open System
open System.Collections
open System.Collections.Generic
open System.Collections.Concurrent
open System.Reflection
open System.Linq
open System.Linq.Expressions
open Microsoft.FSharp.Reflection
open Microsoft.FSharp.Quotations

open Barb.Interop
open Barb.Representation
open Barb.Parse
open Barb.Reduce

open Helpers
open Helpers.FSharpExpr

/// Internal compiler parts used by the REPL
module CompilerParts =

    /// Parses the input string 
    let parse (settings: BarbSettings) (predicate: string) : BarbData = 
        
        let parsedTokens = parseProgram predicate

        #if DEBUG
        let ptOutput = Environment.NewLine + (sprintf "PT: %A" parsedTokens) + Environment.NewLine in
            System.Diagnostics.Debug.WriteLine(ptOutput)
        #endif

        { BarbData.Default with Contents = [parsedTokens]; Settings = settings }

    /// Reduces/optimizes the expression tree
    let reduce (data: BarbData) : BarbData =        
        let memberMap =
            resolveMembers data.InputType (BindingFlags.Instance ||| BindingFlags.Static ||| BindingFlags.NonPublic ||| BindingFlags.Public)
            |> Seq.map (fun (name,contents) -> name, ComingLater)

        let moduleBindings = 
            getModulesByNamespaceName data.Settings.Namespaces
            |> Seq.collect getContentsFromModule 
            |> Seq.map (fun (n,lexpr) -> n, Existing <| lexpr)
        
        let additionalBindings =
            data.Settings.AdditionalBindings |> Seq.map (fun kv -> kv.Key, wrapExistingBinding (Obj kv.Value))
            
        let allBindings = 
            // memberMap last to ensure those overwrite anything conflicting from the others
            seq { yield! moduleBindings; yield! additionalBindings; yield! memberMap }        
            |> Map.ofSeq

        let reducedExpression = 
            resolveExpression data.Contents allBindings data.Settings false |> fst |> List.rev

        #if DEBUG
        let reOutput = Environment.NewLine + (sprintf "RE: %A" reducedExpression) + Environment.NewLine in
            System.Diagnostics.Debug.WriteLine(reOutput)
        #endif
        
        { data with Contents = reducedExpression }

    /// Performs the 'final reduction'. This is the execution phase.
    let reduceFinal (context: Bindings) (parsed: BarbData) : ExprRep list * Bindings =
        resolveExpression parsed.Contents context parsed.Settings true

    /// Sets the input type of the Barb function
    let setInput (inputType: Type) (data: BarbData) : BarbData =
        { data with InputType = inputType }

    /// Sets the output type of the Barb function
    let setOutput (outputType: Type) (data: BarbData) : BarbData =
        { data with OutputType = outputType }

    /// Converts the Barb representation to an actual function callable in .NET
    let toFunction (data: BarbData) : (obj -> obj) =
        let memberMap =
            resolveMembers data.InputType (BindingFlags.Instance ||| BindingFlags.Static ||| BindingFlags.NonPublic ||| BindingFlags.Public)
            |> Map.ofSeq 

        let additionalBindings =
            data.Settings.AdditionalBindings |> Seq.map (fun kv -> kv.Key, wrapExistingBinding (Obj kv.Value ))

        let calculateResult input = 
            let applyToMember (mi: MethodInfo list) = lazy (AppliedMethod(input, mi))
            let inputBindings = memberMap |> Map.map (fun k prop -> prop input |> wrapExistingBinding)

            #if DEBUG
            let ibOutput = Environment.NewLine + (sprintf "IB: %A" inputBindings) + Environment.NewLine in
                System.Diagnostics.Debug.WriteLine(ibOutput)
            #endif

            resolveExpression data.Contents inputBindings data.Settings true
            |> fst |> resolveExpressionResult

        fun input -> 
            match calculateResult input with
            | null -> null
            | result when result.GetType() = data.OutputType -> result
            | result ->
                result
                |> convertToTargetType data.OutputType 
                |> function 
                   | Some (typedRes) -> typedRes       
                   | None -> result

module Compiler = 
    open CompilerParts

    /// Creates an untyped Barb function (obj -> obj) with the given settings
    let buildUntypedExprWithSettings inputType outputType settings predicate = 
        let buildFunction = parse settings >> reduce >> setInput inputType >> setOutput outputType >> toFunction
        let expression = buildFunction predicate 
        fun (input : obj) -> 
            let res = expression input
            match res with
            | null -> null
            | result when result.GetType() = outputType -> result
            | untypedRes -> convertToTargetType outputType untypedRes 
                            |> function 
                               | Some (typedRes) -> typedRes           
                               | None -> failwith (sprintf "Unexpected output type/format: %A/%s" untypedRes (untypedRes.GetType().Name))

    /// Creates a typed Barb function expression with the given settings
    let buildExprWithSettings<'I, 'O> settings predicate =
        let utfun = buildUntypedExprWithSettings typeof<'I> typeof<'O> settings predicate 
        fun (input: 'I) -> (utfun input) :?> 'O

    /// Creates a typed Barb function with the default settings
    let buildExpr<'I,'O> predicate =
        buildExprWithSettings<'I,'O> BarbSettings.Default predicate  

    /// Creates an untyped Barb Expression (F# Expr) with the given settings
    let buildAsExprWithSettings intype outtype settings predicate = 
        let utfun = buildUntypedExprWithSettings intype outtype settings predicate         
        let arg = Var("x", intype, false)
        let useArg = Expr.Var(arg)
        let contents = useArg |> coerse typeof<obj> |> application <@ utfun @> |> coerse outtype
        Expr.Lambda(arg, contents)

/// A helper class to make creating Typed Barb functions easy in C#
type BarbFunc<'I,'O> (predicate, ?settings) =
    let settings = defaultArg settings BarbSettings.Default
    let func : 'I -> 'O = Compiler.buildExprWithSettings settings (predicate)
    member t.Execute (record: 'I) =     
        func record

/// A helper class to make creating Untyped Barb functions easy in C#
type BarbFuncUntyped (inputType, outputType, predicate, ?settings) =
    let settings = defaultArg settings BarbSettings.Default
    let func : obj -> obj = Compiler.buildUntypedExprWithSettings inputType outputType settings (predicate)
    member t.Execute (record: obj) =     
        func record
