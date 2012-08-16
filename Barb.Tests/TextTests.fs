module Barb.Tests.TextTests

open Barb.Compiler

open Xunit

type DudeRecord = { Name: string; Sex: char }

[<Fact>] 
let ``should support quoted values`` () =
    let testRecord = { Name = "Dude Duderson"; Sex = 'f' }
    let predicate = buildExpr<DudeRecord,bool> "Name = \"Dude Duderson\" and Sex = 'f'"
    let result = predicate testRecord
    Assert.True(result)

[<Fact>] 
let ``should support quoted values even when they contain a period`` () =
    let testRecord = { Name = "Dude.Duderson"; Sex = 'f' }
    let predicate = buildExpr<DudeRecord, bool> "Name = \"Dude.Duderson\" and Sex = 'f'"
    let result = predicate testRecord
    Assert.True(result)

[<Fact>] 
let ``should support escaped quotes`` () =
    let testRecord = { Name = "Dude\"Duderson"; Sex = 'f' }
    let predicate = buildExpr<DudeRecord,bool> "Name = \"Dude\\\"Duderson\" and Sex = 'f'"
    let result = predicate testRecord
    Assert.True(result)