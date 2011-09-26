module InternalsTests

open Barb.Internals

open Xunit

type testLayerOne = { One: int; Two: int }
type testLayerTwo = { LOne: testLayerOne; Name: string }

let getProperyOrFail lookupCache name = 
    match lookupCache name with
    | Some (PropertyCall call) -> call
    | badResult -> failwith (sprintf "Bad result in propety lookup: %A" badResult)

let getCachedMembers ttype =     
    let memberMap = resolveAllProperties ttype "" id |> Map.ofSeq
    fun memberName -> 
        memberMap |> Map.tryFind memberName

[<Fact>]
let ``getCachedProperties should work on single depth lookup`` () =
    let lookupCache = getCachedMembers typeof<testLayerOne>
    let testInstance = { One = 1; Two = 2 } :> obj
    Assert.Equal(1, (getProperyOrFail lookupCache "One") testInstance :?> int)
    Assert.Equal(2, (getProperyOrFail lookupCache "Two") testInstance :?> int)

[<Fact>]
let ``tokenizeString should correctly tokenize a simple string`` () =
    let tokens = tokenizeString "Hello = World"
    Assert.Equal(3, tokens.Length)
    Assert.Equal(("Hello", Normal), tokens.[0])
    Assert.Equal(("=", Normal), tokens.[1])
    Assert.Equal(("World", Normal), tokens.[2])

[<Fact>]
let ``tokenizeString should correctly tokenize a string with quotes`` () =
    let tokens = tokenizeString "\"Good Morning\" = \"World News\""
    Assert.Equal(3, tokens.Length)
    Assert.Equal(("Good Morning", Quoted), tokens.[0])
    Assert.Equal(("=", Normal), tokens.[1])
    Assert.Equal(("World News", Quoted), tokens.[2])
