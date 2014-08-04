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

let returnsecond (a: 'a) (b: 'b) = b
    
[<Fact>]
let ``Barb should be able to call simple generic functions with two parameters one generic`` () =
    let namespaces = BarbSettings.Default.Namespaces |> Set.add "Barb.Tests.PredicateLanguageGenericInteropTests"
    let settings = { BarbSettings.Default with Namespaces = namespaces } 

    let pred = "returnsecond 10 \"Hello\""

    let func = new BarbFunc<unit,string>(pred, settings)
    let res = func.Execute()
    Assert.Equal<string>("Hello", res)

open System.Collections.Generic

let isCountEqualTo (l: IEnumerable<'T>) (len: int) =
    (l |> Seq.length) = len

//[<Fact>]
let ``Barb should be able to call a function with a multi parameter nested generic`` () =
    let namespaces = BarbSettings.Default.Namespaces |> Set.add "Barb.Tests.PredicateLanguageGenericInteropTests"
    let settings = { BarbSettings.Default with Namespaces = namespaces } 

    let pred = "isCountEqualTo ((10, 20, 30, 40), 4)"

    let func = new BarbFunc<unit,bool>(pred, settings)
    let res = func.Execute()
    Assert.Equal<bool>(true, res)

let seqHead (l: IEnumerable<'T>) = l.GetEnumerator().Current

//[<Fact>]
let ``Barb should be able to call a function with a single parameter nested generic`` () =
    let namespaces = BarbSettings.Default.Namespaces |> Set.add "Barb.Tests.PredicateLanguageGenericInteropTests"
    let settings = { BarbSettings.Default with Namespaces = namespaces } 

    let pred = "seqHead (10, 20, 30, 40)"

    let func = new BarbFunc<unit,int>(pred, settings)
    let res = func.Execute()
    Assert.Equal<int>(10, res)

  