module Barb.Tests.Lib

open Barb.Compiler
open Xunit

type ArrayContainer<'T> = { Stuff: 'T array }

[<Fact>] 
let ``loadTSVTable should properly load a tsv table`` () =
    let predicate = buildExpr<unit,bool> "let table = Table.loadTSV 'TSVTableTestFile.tsv' in table = [|[|'one';'two';'three'|]; [|'four';'five';'six'|]; [|'one';'five'|]|]"
    let result = predicate ()
    Assert.True(result)

[<Fact>] 
let ``firstRowContaining should properly select a contained row`` () =
    let predicate = buildExpr<unit,bool> "let table = Table.loadTSV 'TSVTableTestFile.tsv' in Table.firstRowContaining(table,'one') = [|'one';'two';'three'|]"
    let result = predicate ()
    Assert.True(result)

[<Fact>] 
let ``allRowsContaining should properly select all contained rows`` () =
    let predicate = buildExpr<unit,bool> "let table = Table.loadTSV 'TSVTableTestFile.tsv' in Table.allRowsContaining(table,'one') = [|[|'one';'two';'three'|]; [|'one';'five'|]|]"
    let result = predicate ()
    Assert.True(result)