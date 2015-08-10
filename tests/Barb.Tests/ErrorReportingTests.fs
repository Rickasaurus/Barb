module ErrorReportingTests

open System

open Barb
open Barb.Compiler
open Barb.Parse
open Barb.Reduce
open Barb.Representation

open Xunit

type NestedRec = { Hello: string }
    with member t.GetHello() = t.Hello
         member t.GetHelloPlus(str)= t.Hello + " " + str

type MiscRecord = { Name: string; Money: int; Greeting: NestedRec }

let showOffset (code: string) (offset: uint32) (length: uint32) =
    code.Insert(int offset, "@").Insert(int (offset + length + 1u), "@")

[<Fact>]
let ``should throw an appropriate exception when a variable is not found`` () = 
    let testRecord = { Name = "The Dude"; Money = 9000; Greeting = { Hello = "Yes" }}
    let code = "Nam = 'The Dude'"
    let predicate = buildExpr<MiscRecord,bool> code
    try
        predicate testRecord |> ignore
        Assert.True(false)
    with | (:? BarbException as ex) -> 
        Assert.Equal<String>("Specified unknown was unable to be resolved: Nam", ex.Message)
        Assert.Equal<String>("@Nam@ = 'The Dude'", showOffset code ex.Offset ex.Length)

[<Fact>]
let ``should throw an appropriate exception when a member is not found`` () = 
    let testRecord = { Name = "The Dude"; Money = 9000; Greeting = { Hello = "Yes" }}
    let code = "Greeting.Helo = 'Yes'"
    let predicate = buildExpr<MiscRecord,bool> code
    try
        predicate testRecord |> ignore
        Assert.True(false)
    with | (:? BarbException as ex) -> 
        Assert.Equal<String>("Unable to find member Helo in object of type NestedRec", ex.Message)
        Assert.Equal<String>("Greeting@.Helo@ = 'Yes'", showOffset code ex.Offset ex.Length)

[<Fact>]
let ``should throw an appropriate exception when a unit method is not found`` () = 
    let testRecord = { Name = "The Dude"; Money = 9000; Greeting = { Hello = "Yes" }}
    let code = "Greeting.GetHelo() = 'Yes'"
    let predicate = buildExpr<MiscRecord,bool> code
    try
        predicate testRecord |> ignore
        Assert.True(false)
    with | (:? BarbException as ex) -> 
        Assert.Equal<String>("Unable to find member GetHelo in object of type NestedRec", ex.Message)
        Assert.Equal<String>("Greeting@.GetHelo@() = 'Yes'", showOffset code ex.Offset ex.Length)

[<Fact>]
let ``should throw an appropriate exception when a single var method is not found`` () = 
    let testRecord = { Name = "The Dude"; Money = 9000; Greeting = { Hello = "Yes" }}
    let code = "Greeting.GetHeloPlus ('sir') = 'Yes sir'"
    let predicate = buildExpr<MiscRecord,bool> code
    try
        predicate testRecord |> ignore
        Assert.True(false)
    with | (:? BarbException as ex) -> 
        Assert.Equal<String>("Unable to find member GetHeloPlus in object of type NestedRec", ex.Message)
        Assert.Equal<String>("Greeting@.GetHeloPlus@ ('sir') = 'Yes sir'", showOffset code ex.Offset ex.Length)

[<Fact>]
let ``should throw an appropriate exception a barb variable is not found`` () = 
    let testRecord = { Name = "The Dude"; Money = 9000; Greeting = { Hello = "Yes" }}
    let code = "let ex = 'sir' in Greeting.GetHelloPlus x = 'Yes sir'"
    let predicate = buildExpr<MiscRecord,bool> code
    try
        predicate testRecord |> ignore
        Assert.True(false)
    with | (:? BarbException as ex) -> 
        Assert.Equal<String>("Specified unknown was unable to be resolved: x", ex.Message)
        Assert.Equal<String>("let ex = 'sir' in Greeting.GetHelloPlus @x@ = 'Yes sir'", showOffset code ex.Offset ex.Length)

type TestCtr(str: string) =
    member t.Contents = str
type HostTestCtr = { Test: TestCtr }

[<Fact>]
let ``should throw an appropriate exception when a constructor type cannot be found`` () = 
    let testRecord = { Test = TestCtr "The One" }
    let code = "Test = new TestCt('The One')"
    let predicate = buildExpr<HostTestCtr,bool> code
    try
        predicate testRecord |> ignore
        Assert.True(false)
    with | (:? BarbException as ex) -> 
        Assert.Equal<String>("Specified unknown was unable to be resolved: TestCt", ex.Message)
        Assert.Equal<String>("Test = @new TestCt@('The One')", showOffset code ex.Offset ex.Length)

type ColElement = { E: string }
type ColWrapper = { C: ColElement list }

[<Fact>]
let ``should throw an appropriate exception calling a non-existing property on a collection`` () = 
    let testRecord = { C = [ {E = "One"}; {E = "Two"}; {E = "Three"} ] }
    let code = "C..Hello = ('One','Two','Three')"
    let predicate = buildExpr<ColWrapper,bool> code
    try
        predicate testRecord |> ignore
        Assert.True(false)
    with | (:? BarbException as ex) -> 
        Assert.Equal<String>("Unable to find member Hello in object of type ColElement", ex.Message)
        Assert.Equal<String>("C@..Hello@ = ('One','Two','Three')", showOffset code ex.Offset ex.Length)

let failFun () = failwith "This function failed."; 0

[<Fact>]
let ``should properly report an exception thrown inside of a called function`` () =
    let settings = { BarbSettings.Default with Namespaces = BarbSettings.Default.Namespaces |> Set.add "ErrorReportingTests" }
    let code = "failFun()"
    try 
        let func = new BarbFunc<unit,int>(code, settings)
        func.Execute() |> ignore
        Assert.True(false)
    with | (:? BarbExecutionException as ex) ->
        Assert.Equal<String>("Exception occured while invoking a method: This function failed.", ex.Message)
        Assert.Equal<String>("@failFun@()", showOffset code ex.Offset ex.Length)

[<Fact>]
let ``should properly report an exception thrown inside of a called unit function`` () =
    let settings = { BarbSettings.Default with Namespaces = BarbSettings.Default.Namespaces |> Set.add "ErrorReportingTests" }
    let code = "failFun ()"
    try 
        let func = new BarbFunc<unit,int>(code, settings)
        func.Execute() |> ignore
        Assert.True(false)
    with | (:? BarbExecutionException as ex) ->
        Assert.Equal<String>("Exception occured while invoking a method: This function failed.", ex.Message)
        Assert.Equal<String>("@failFun@ ()", showOffset code ex.Offset ex.Length)
