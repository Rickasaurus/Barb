module Barb.Tests.PredicateLanguageGenericInteropTests

open System

open Barb
open Barb.Compiler
open Barb.Representation

open Xunit

let returnit (v: 'a) = v

[<Fact>]
let ``Barb should be able to call simple generic functions`` () =
    let namespaces = BarbSettings.Default.Namespaces |> Set.add "Barb.Tests.PredicateLanguageGenericInteropTests"
    let settings = { BarbSettings.Default with Namespaces = namespaces } 

    let pred = "returnit 10"

    let func = new BarbFunc<unit,int>(pred, settings)
    let res = func.Execute()
    Assert.Equal(10, res)
    