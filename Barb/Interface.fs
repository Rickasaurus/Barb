namespace Barb

// TODO:
// Add option to disable optimization (for mutable interactions)
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

    let buildExpression (localType: Type) (predicate: string) (settings: BarbSettings) : (obj -> obj) =

        let memberMap =
            resolveMembers localType (BindingFlags.Instance ||| BindingFlags.Static ||| BindingFlags.NonPublic ||| BindingFlags.Public)
            |> Map.ofSeq
        
        let additionalBindings =
            settings.AdditionalBindings |> Seq.map (fun kv -> kv.Key, lazy (Obj kv.Value))
              
        let parsedTokens = parseProgram predicate

    #if DEBUG
        printfn ""
        printfn "PT: %A" parsedTokens
        printfn ""
    #endif

        let reducedExpression = 
            if settings.OptimizeForImmutability then
                resolveExpression [parsedTokens] Map.empty false |> List.rev
            else [parsedTokens]

    #if DEBUG
        printfn ""
        printfn "RE: %A" reducedExpression
        printfn ""
    #endif

        let calculateResult input = 
            let inputBindings =
                additionalBindings               
                |> Seq.fold (fun s (k,v) -> s |> Map.add k v )
                    (memberMap |> Map.map (fun k prop -> lazy (prop input)))
    
    #if DEBUG
            printfn ""
            printfn "IB: %A" inputBindings
            printfn ""
    #endif

            resolveExpression reducedExpression inputBindings true
            |> resolveExpressionResult

        calculateResult

    let buildExprWithSettings<'I, 'O> settings predicate =
        let expression = buildExpression typeof<'I> predicate settings
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

