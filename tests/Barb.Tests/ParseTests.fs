module Barb.Tests.ParseTests


open Barb.Parse
open Xunit

[<Fact>]
let ``StringWindow should correctly Subwindow`` () =
    let str = "hello"
    let strwin = StringWindow(str, 0u)

    let substr = str.Substring(1)
    let subwin = strwin.Subwindow(1u)    
    Assert.Equal<string>(substr, subwin.ToString())

    let substr = str.Substring(str.Length - 1)
    let subwin = strwin.Subwindow(strwin.Length - 1u)    
    Assert.Equal<string>(substr, subwin.ToString())


[<Fact>]
let ``StringWindow should correctly Substring`` () =
    let str = "hello"
    let strwin = StringWindow(str, 0u)

    let substr = str.Substring(1, 2)
    let subwin = strwin.Substring(1u, 2u)
    Assert.Equal<string>(substr, subwin)

    let substr = str.Substring(str.Length - 1, 1)
    let subwin = strwin.Substring(strwin.Length - 1u, 1u)
    Assert.Equal<string>(substr, subwin) 

[<Fact>]
let ``StringWindow should correctly Length`` () =
    let str = "hello"
    let strlen = str.Substring(1).Length
    let winlen = StringWindow(str, 1u).Length |> int
    Assert.Equal(strlen, winlen)


[<Fact>]
let ``StringWindow should correctly IndexOf`` () =
    let str = "1234567890"
    let sw = StringWindow(str, 0u)
    let stridx = str.IndexOf("1")
    let swidx = sw.IndexOf("1") |> int
    Assert.Equal(stridx, swidx)

    let substr = str.Substring(2)
    let subsw = sw.Subwindow(2u)
    let stridx = substr.IndexOf("56")
    let swidx = subsw.IndexOf("56") |> int
    Assert.Equal(stridx, swidx)

    let substr = str.Substring(9)
    let subsw = sw.Subwindow(9u)
    let stridx = substr.IndexOf("0")
    let swidx = subsw.IndexOf("0") |> int
    Assert.Equal(stridx, swidx)

    let substr = str.Substring(10)
    let subsw = sw.Subwindow(10u)
    let stridx = substr.IndexOf("0")
    let swidx = subsw.IndexOf("0") |> int
    Assert.Equal(stridx, swidx)

[<Fact>]
let ``StringWindow should correctly Index`` () =
    let str = "1234567890"
    let sw = StringWindow(str, 0u)

    let substr = str.Substring(2)
    let subsw = sw.Subwindow(2u)

    Assert.Equal (substr.[0], subsw.[0u])

[<Fact>]
let ``StringWindow should correctly StartsWith`` () =
    let str = "1234567890"
    let sw = StringWindow(str, 0u)

    Assert.True(str.StartsWith(str))
    Assert.True(sw.StartsWith(str))
    Assert.True(str.StartsWith("1"))
    Assert.True(sw.StartsWith("1"))

    let substr = str.Substring(2)    
    let subsw = sw.Subwindow(2u)
    Assert.True(subsw.StartsWith(substr))

    Assert.True(substr.StartsWith("34"))
    Assert.True(subsw.StartsWith("34"))
    Assert.False(substr.StartsWith("45"))
    Assert.False(subsw.StartsWith("45"))
    Assert.True(substr.StartsWith("3"))
    Assert.True(subsw.StartsWith("3"))
    Assert.False(substr.StartsWith("4"))
    Assert.False(subsw.StartsWith("4"))
    Assert.False(substr.StartsWith("2"))
    Assert.False(subsw.StartsWith("2"))

[<Fact>]
let ``StringWindow should correctly StartsWith -- regresssion`` () =
    let str = "true and true"
    let sw = StringWindow(str, 8u)
    
    Assert.Equal<string>(sw.ToString(), " true")
    Assert.True(sw.StartsWith(" "))    