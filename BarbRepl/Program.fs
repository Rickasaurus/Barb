open System
open Barb.Representation
open Barb.Compiler

let printHighlightedError (input: string) (ex: BarbException) =
    let initialColor = System.Console.ForegroundColor
    do System.Console.Write(" ")
       System.Console.Write (input.Substring(0, int ex.Offset))
       System.Console.ForegroundColor <- ConsoleColor.Red
       System.Console.Write (input.Substring(int ex.Offset, int ex.Length))
       System.Console.ForegroundColor <- initialColor
       System.Console.WriteLine (input.Substring(int <| ex.Offset + ex.Length))


[<EntryPoint>]
let main argv = 
    printfn " Welcome to the Barb REPL. Type #help for a list of the REPL options."

    let lastCmd = ref ""
    let lastResult = ref None
    let settings = BarbSettings.Default

    let context = ref Map.empty
    let lastResult = ref []

    let showParsed = ref false
    let showOptimized = ref false
    let showContext = ref false
    let exitRepl = ref false

    let parseCommands =
        function
        | "#showparsed" -> 
            showParsed := not !showParsed
            printfn " Parsed Structure will be printed: %A" !showParsed
            true
        | "#showoptimized" ->
            showOptimized := not !showOptimized
            printfn " Optimized Structure will be printed: %A" !showOptimized
            true            
        | "#showcontext" ->
            showContext := not !showContext
            printfn " Context will be printed: %A" !showContext
            true                 
        | "#exit" -> exitRepl := true; true
        | "#help" ->
            printfn " #showparsed    : Show internal representation after parsing"
            printfn " #showoptimized : Show internal representation after optimization"
            printfn " #showcontext   : Show values context after execution"
            printfn " #exit          : Exit from the Barb REPL program"
            printfn " #help          : This message"
            printfn ""
            true
        | _ -> false

    while not !exitRepl do
        printf "Barb> "
        lastCmd := System.Console.ReadLine()
        let highlightedErrorFunc = printHighlightedError !lastCmd
        if parseCommands ((!lastCmd).ToLowerInvariant()) then ()
        else
            let parsed = 
                try parse settings !lastCmd |> Some
                with :? BarbException as ex ->
                           printfn "Error in Parse: %s" ex.Message
                           highlightedErrorFunc ex; None
            if !showParsed then
                parsed |> Option.iter (fun parsed -> 
                    printfn "---- Parsed Representation ----"
                    printf " %A" parsed.Contents
                    printfn "-------------------------------")
            let optimzied = 
                try parsed |> Option.map (fun parsed -> reduce parsed)
                with :? BarbException as ex ->
                           printfn "Error in Optimization: %s" ex.Message; 
                           highlightedErrorFunc ex; None
            if !showOptimized then
                optimzied |> Option.iter (fun optimzied -> 
                    printfn "---- Optimized Representation ----"
                    printf " %A" optimzied.Contents
                    printfn "----------------------------------")
            let final = 
                try optimzied |> Option.map (fun optimized -> reduceFinal !context optimized) 
                with :? BarbException as ex -> 
                           printfn "Error in Execution: %s" ex.Message
                           highlightedErrorFunc ex; None
            match final with
            | Some (lr, lc) -> 
                lastResult := lr; context := lc
                printfn " %A" (lr |> List.map (fun r -> r.Expr))
                printfn ""
            | None -> ()
            if !showContext then 
                printfn "---------- Context ----------"
                printf " %A" !context
                printfn "-----------------------------"       

    0 // return an integer exit code
