module Barb.Tests.FSharpInteropTests

open System

open Barb
open Barb.Compiler
open Barb.Representation

open Xunit

module TestModule1 = 
    let one = 1
    let addOne (n: int) = n + 1

let two = 2
let addTwo (n: int) = n + 2

type IntRec = { Num: int }

[<Fact>]
let ``Barb should be able to open a module as if it were a namespace and access a value`` () =
    let namespaces = BarbSettings.Default.Namespaces |> Set.add "Barb.Tests.FSharpInteropTests.TestModule1"
    let settings = { BarbSettings.Default with Namespaces = namespaces } 
    let pred = "one + 1"
    let func = new BarbFunc<IntRec,int>(pred, settings)
    let res = func.Execute({ Num = 1 })
    Assert.Equal(2, res)

[<Fact>]
let ``Barb should be able to open a top level module as if it were a namespace and access a value`` () =
    let namespaces = BarbSettings.Default.Namespaces |> Set.add "Barb.Tests.FSharpInteropTests"
    let settings = { BarbSettings.Default with Namespaces = namespaces } 
    let pred = "two + 1"
    let func = new BarbFunc<IntRec,int>(pred, settings)
    let res = func.Execute({ Num = 1 })
    Assert.Equal(3, res)

[<Fact>]
let ``Barb should be able to open a module as if it were a namespace and access a function`` () =
    let namespaces = BarbSettings.Default.Namespaces |> Set.add "Barb.Tests.FSharpInteropTests.TestModule1"
    let settings = { BarbSettings.Default with Namespaces = namespaces } 
    let pred = "addOne Num"
    let func = new BarbFunc<IntRec,int>(pred, settings)
    let res = func.Execute({ Num = 1 })
    Assert.Equal(2, res)

[<Fact>]
let ``Barb should be able to open a top level module as if it were a namespace and access a function`` () =
    let namespaces = BarbSettings.Default.Namespaces |> Set.add "Barb.Tests.FSharpInteropTests"
    let settings = { BarbSettings.Default with Namespaces = namespaces } 
    let pred = "addTwo Num"
    let func = new BarbFunc<IntRec,int>(pred, settings)
    let res = func.Execute({ Num = 1 })
    Assert.Equal(3, res)
