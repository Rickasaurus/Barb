module InternalsTests

open Barb.Internals

open Xunit

type testLayerOne = { One: int; Two: int }
type testLayerTwo = { LOne: testLayerOne; Name: string }

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
