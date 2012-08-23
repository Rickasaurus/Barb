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

    let buildExprWithSettings<'I, 'O> settings predicate =
        let buildFunction = parse settings >> reduce >> setInput typeof<'I> >> setOutput typeof<'O> >> toFunction
        let expression = buildFunction predicate 
        fun (input : 'I) -> 
            match expression input with
            | :? 'O as result -> result
            | untypedRes -> convertToTargetType typeof<'O> untypedRes 
                            |> function 
                               | Some (typedRes) -> typedRes :?> 'O               
                               | None -> failwith (sprintf "Unexpected output type/format: %A/%s" untypedRes (untypedRes.GetType().Name))

    let buildExpr<'I,'O> predicate =
        buildExprWithSettings<'I,'O> BarbSettings.Default predicate   

type BarbFunc<'I,'O> (predicate, ?settings) =
    let settings = defaultArg settings BarbSettings.Default
    let func : 'I -> 'O = Compiler.buildExprWithSettings settings (predicate)
    member t.Execute (record: 'I) =     
        func record

