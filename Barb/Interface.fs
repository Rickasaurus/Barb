namespace Barb

// TODO:
// Add "global world" bindings which can be optimzied 

open System
open System.Collections
open System.Collections.Generic
open System.Collections.Concurrent
open System.Reflection

open Barb.Interop
open Barb.Representation
open Barb.Parse
open Barb.Reduce

module Compiler =

    let parse (settings: BarbSettings) (predicate: string) : BarbData = 
        
        let parsedTokens = parseProgram predicate

        #if DEBUG
        let ptOutput = Environment.NewLine + (sprintf "PT: %A" parsedTokens) + Environment.NewLine in
            System.Diagnostics.Debug.WriteLine(ptOutput)
        #endif

        { BarbData.Default with Contents = [parsedTokens]; Settings = settings }

    let reduce (parsed: BarbData) : BarbData =

        let additionalBindings =
            parsed.Settings.AdditionalBindings |> Seq.map (fun kv -> kv.Key, lazy (Obj kv.Value))

        let reducedExpression = 
            resolveExpression parsed.Contents Map.empty parsed.Settings false |> fst |> List.rev

        #if DEBUG
        let reOutput = Environment.NewLine + (sprintf "RE: %A" reducedExpression) + Environment.NewLine in
            System.Diagnostics.Debug.WriteLine(reOutput)
        #endif
           
        { parsed with Contents = reducedExpression }

    let reduceFinal (context: Bindings) (parsed: BarbData) : ExprRep list * Bindings =
        resolveExpression parsed.Contents context parsed.Settings true

    let setInput (inputType: Type) (data: BarbData) : BarbData =
        { data with InputType = inputType }

    let setOutput (outputType: Type) (data: BarbData) : BarbData =
        { data with OutputType = outputType }

    let toFunction (data: BarbData) : (obj -> obj) =
        let memberMap =
            resolveMembers data.InputType (BindingFlags.Instance ||| BindingFlags.Static ||| BindingFlags.NonPublic ||| BindingFlags.Public)
            |> Map.ofSeq
        
        let additionalBindings =
            data.Settings.AdditionalBindings |> Seq.map (fun kv -> kv.Key, lazy (Obj kv.Value))
                    
        let calculateResult input = 
            let inputBindings =
                additionalBindings               
                |> Seq.fold (fun s (k,v) -> s |> Map.add k v )
                    (memberMap |> Map.map (fun k prop -> lazy (prop input)))        

            #if DEBUG
            let ibOutput = Environment.NewLine + (sprintf "IB: %A" inputBindings) + Environment.NewLine in
                System.Diagnostics.Debug.WriteLine(ibOutput)
            #endif

            resolveExpression data.Contents inputBindings data.Settings true
            |> fst |> resolveExpressionResult

        fun input -> 
            let naiveResult = calculateResult input
            naiveResult
            |> convertToTargetType data.OutputType 
            |> function 
               | Some (typedRes) -> typedRes       
               | None -> naiveResult

    let buildUntypedExprWithSettings inputType outputType settings predicate = 
        let buildFunction = parse settings >> reduce >> setInput inputType >> setOutput outputType >> toFunction
        let expression = buildFunction predicate 
        fun (input : obj) -> 
            match expression input with
            | null -> null
            | result when result.GetType() = outputType -> result
            | untypedRes -> convertToTargetType outputType untypedRes 
                            |> function 
                               | Some (typedRes) -> typedRes           
                               | None -> failwith (sprintf "Unexpected output type/format: %A/%s" untypedRes (untypedRes.GetType().Name))

    let buildExprWithSettings<'I, 'O> settings predicate =
        let utfun = buildUntypedExprWithSettings typeof<'I> typeof<'O> settings predicate 
        fun (input: 'I) -> (utfun input) :?> 'O

    let buildExpr<'I,'O> predicate =
        buildExprWithSettings<'I,'O> BarbSettings.Default predicate  

type BarbFunc<'I,'O> (predicate, ?settings) =
    let settings = defaultArg settings BarbSettings.Default
    let func : 'I -> 'O = Compiler.buildExprWithSettings settings (predicate)
    member t.Execute (record: 'I) =     
        func record

type UntypedBarbFunc (inputType, outputType, predicate, ?settings) =
    let settings = defaultArg settings BarbSettings.Default
    let func : obj -> obj = Compiler.buildUntypedExprWithSettings inputType outputType settings (predicate)
    member t.Execute (record: obj) =     
        func record
