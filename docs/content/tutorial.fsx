(*** hide ***)
// This block of code is omitted in the generated HTML documentation. Use 
// it to define helpers that you do not want to show in the documentation.
#I "../../bin/Barb"

(**
Introducing Barb
========================

The first thing you need to know about Barb is how to generate a function. This is done by calling the buildExpr function with the types you expect as well as your expected result.

*)
#r "Barb.dll"
open Barb.Compiler

let func = buildExpr<unit, string>("'Hello World'")
let result = func ()
printfn "%s" result // "Hello World"

(**
Barb was originally designed for queries on DOM-like data trees. Let's start with a simple example.
*)

open System

type MyRecord = 
    {
        Name: string
        Countries: string []
    }

let testRecords = 
    [ { Name = "Rick Minerich";      Countries = [|"US"; "MX"|] }
      { Name = "Isabella Mapletree"; Countries = [|"CA"; "GR"|] } 
      { Name = "Jose Luis";          Countries = [|"MX"; "CA"|] } ]

(**
You can easily query this simple record for various information. To do this efficiently first generate the function and then call it as you'd like over each record.
Members of the input type are automatically put into scope as variables to make this easy.
*)


let rickFun = buildExpr<MyRecord, bool> "Name.ToUpper().Contains('RICK')"

testRecords |> List.filter rickFun
(**
This is where set operators come into play, as they allow you to do simple contains/overlap queries easily. In this example we're using the has-intersection operator.
*)

let countryFun = buildExpr<MyRecord, bool> "Countries /?\ [|'CA'; 'GR'|]"

testRecords |> List.filter countryFun

(**
Sometimes you may want to map a method call over a collection of items. This can be done with the .. syntax (one dot per level of collection).
*)

let dotFun = buildExpr<MyRecord, bool> "Countries..ToLower() /?\ 'us'"

testRecords |> List.filter dotFun

(**
When a Barb fails it tries its best gives you the offset where something went wrong. Here we're going to access a variable that doesn't exist.
This makes it easy to report what went wrong to the user, even when they have a long function string.
*)

open Barb.Representation

let failFunStr = "Cntry /?\ 'Hello'"
try 
    let failFun = buildExpr<MyRecord, bool> failFunStr
    testRecords |> List.map failFun |> ignore
with | :? BarbException as ex -> 
    let failedPart = failFunStr.Substring(int ex.Offset, int ex.Length)
    printfn "Failed on: '%s' with '%s'" failedPart ex.Message

