module Barb.Tests.Lib

open Barb.Compiler
open Xunit

[<Fact>] 
let ``should support hasIntersection`` () =
    let predicate = buildExpr<unit,bool> "Lib.hasIntersection ((1,2,3), (3,4,5))"
    let result = predicate ()
    Assert.True(result)

[<Fact>] 
let ``should properly return false on hasIntersection`` () =
    let predicate = buildExpr<unit,bool> "Lib.hasIntersection ((1,2,3), (4,5))"
    let result = predicate ()
    Assert.False(result)

[<Fact>] 
let ``hasIntersection should work one a single element`` () =
    let predicate = buildExpr<unit,bool> "Lib.hasIntersection ((4,5), (4))"
    let result = predicate ()
    Assert.True(result)

[<Fact>] 
let ``hasIntersection should work one a single string element`` () =
    let predicate = buildExpr<unit,bool> "Lib.hasIntersection (('hello','duder'), ('duder'))"
    let result = predicate ()
    Assert.True(result)

type ArrayContainer<'T> = { Stuff: 'T array }

[<Fact>] 
let ``hasIntersection should work correctly on an external set`` () =
    let arrayContainer = { Stuff = [| 4; 5 |] }
    let predicate = buildExpr<ArrayContainer<int>,bool> "Lib.hasIntersection (Stuff, (4,5))"
    let result = predicate arrayContainer
    Assert.True(result)

[<Fact>] 
let ``hasIntersection should work correctly on an empty set`` () =
    let arrayContainer = { Stuff = [| |] }
    let predicate = buildExpr<ArrayContainer<int>,bool> "Lib.hasIntersection (Stuff, (4,5))"
    let result = predicate arrayContainer
    Assert.False(result)


[<Fact>] 
let ``should support union`` () =
    let predicate = buildExpr<unit,bool> "Lib.union ((1,2,3), (4,5)) = (1,2,3,4,5)"
    let result = predicate ()
    Assert.True(result)

[<Fact>] 
let ``should support intersection`` () =
    let predicate = buildExpr<unit,bool> "Lib.intersection ((1,2,3), (4,2,3)) = (2,3)"
    let result = predicate ()
    Assert.True(result)

[<Fact>] 
let ``loadTSVTable should properly load a tsv table`` () =
    let predicate = buildExpr<unit,bool> "let table = Table.loadTSV 'TSVTableTestFile.tsv' in table = (('one','two','three'),('four','five','six'),('one','five'))"
    let result = predicate ()
    Assert.True(result)

[<Fact>] 
let ``firstRowContaining should properly select a contained row`` () =
    let predicate = buildExpr<unit,bool> "let table = Table.loadTSV 'TSVTableTestFile.tsv' in Table.firstRowContaining(table,'one') = ('one','two','three')"
    let result = predicate ()
    Assert.True(result)

[<Fact>] 
let ``allRowsContaining should properly select all contained rows`` () =
    let predicate = buildExpr<unit,bool> "let table = Table.loadTSV 'TSVTableTestFile.tsv' in Table.allRowsContaining(table,'one') = (('one','two','three'),('one','five'))"
    let result = predicate ()
    Assert.True(result)