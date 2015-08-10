module Barb.Tests.FSharpInteropTests

open System
open System.IO

open Barb
open Barb.Compiler
open Barb.Representation

open Xunit

module TestModule1 = 
    let one = 1
    let addOne (n: int) = n + 1
    let addTwoNumbers (n: int) (m: int) = n + m

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
let ``Barb should be able to open a module as if it were a namespace and access a curried function`` () =
    let namespaces = BarbSettings.Default.Namespaces |> Set.add "Barb.Tests.FSharpInteropTests.TestModule1"
    let settings = { BarbSettings.Default with Namespaces = namespaces } 
    let pred = "addTwoNumbers Num 10"
    let func = new BarbFunc<IntRec,int>(pred, settings)
    let res = func.Execute({ Num = 1 })
    Assert.Equal(11, res)


[<Fact>]
let ``Barb should be able to open a top level module as if it were a namespace and access a function`` () =
    let namespaces = BarbSettings.Default.Namespaces |> Set.add "Barb.Tests.FSharpInteropTests"
    let settings = { BarbSettings.Default with Namespaces = namespaces } 
    let pred = "addTwo Num"
    let func = new BarbFunc<IntRec,int>(pred, settings)
    let res = func.Execute({ Num = 1 })
    Assert.Equal(3, res)

[<Fact>]
let ``Barb should be able to wrap a result in an option type``() =
    let settings = BarbSettings.Default 
    let pred = "99"
    let func = new BarbFunc<unit,int64 option>(pred, settings)
    let res = func.Execute()
    Assert.Equal(Some 99L, res)

[<Fact>]
let ``Barb should be able to wrap and convert a result in an option type``() =
    let settings = BarbSettings.Default 
    let pred = "99"
    let func = new BarbFunc<unit,int option>(pred, settings)
    let res = func.Execute()
    Assert.Equal(Some 99, res)


let getFirstLine (file: string) = 
    File.ReadLines(file) |> Seq.head

[<Fact>]
let ``Barb should properly load a file only once when returned from a function``() =
    let filename = Path.GetRandomFileName()
    try
        let code = String.Format("getFirstLine '{0}'", filename);
        let settings = { BarbSettings.Default with Namespaces = BarbSettings.Default.Namespaces |> Set.add "Barb.Tests.FSharpInteropTests" }

        let firstContents = [|"one"|] 
        File.WriteAllLines(filename, firstContents)
        Assert.True(File.Exists(filename))

        let bf = BarbFunc<unit,string>(code, settings)
        Assert.Equal<string>("one", bf.Execute())

        let secondContents = [|"two"|] 
        File.Delete(filename)
        File.WriteAllLines(filename, secondContents)
        Assert.Equal<string>("one", bf.Execute())
    finally
        if File.Exists filename then File.Delete filename

type TestEnum = 
    | One = 1
    | Two = 2
    | Three = 3

[<Fact>]
let ``should properly be able to instantiate an enum type`` () =
    let namespaces = BarbSettings.Default.Namespaces |> Set.add "Barb.Tests.FSharpInteropTests"
    let settings = { BarbSettings.Default with Namespaces = namespaces } 
    let func = new BarbFunc<unit,TestEnum>("TestEnum.Two", settings)
    let res = func.Execute() 
    Assert.Equal(TestEnum.Two, res)

type TestEnumArrayRec =
    {
        Enums: TestEnum []
    }

[<Fact>]
let ``should be able to access an array of enum types`` () =
    let namespaces = BarbSettings.Default.Namespaces |> Set.add "Barb.Tests.FSharpInteropTests"
    let settings = { BarbSettings.Default with Namespaces = namespaces } 
    let func = new BarbFunc<TestEnumArrayRec,TestEnum>("Enums.[1]", settings)
    let res = func.Execute({ Enums = [|TestEnum.One; TestEnum.Two; TestEnum.Three|] }) 
    Assert.Equal(TestEnum.Two, res)
