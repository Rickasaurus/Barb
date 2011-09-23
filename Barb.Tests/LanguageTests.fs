module PredicateLanguage

open Barb.Compiler

open Xunit

type DudeRecord = { Name: string; Sex: char }

[<Fact>] 
let ``predicate language should work with a simple predicate`` () =
    let testRecord = { Name = "Dude"; Sex = 'm' }
    let dudePredicate = buildExpr<DudeRecord,bool> "Name = Dude"
    let result = dudePredicate testRecord
    Assert.True(result)

[<Fact>] 
let ``predicate language should work with a compound predicate`` () =
    let testRecord = { Name = "Dude"; Sex = 'f' }
    let dudePredicate = buildExpr<DudeRecord,bool> "Name = Dude and Sex = f"
    let result = dudePredicate testRecord
    Assert.True(result)

[<Fact>] 
let ``predicate language should support quoted values`` () =
    let testRecord = { Name = "Dude Duderson"; Sex = 'f' }
    let dudePredicate = buildExpr<DudeRecord,bool> "Name = \"Dude Duderson\" and Sex = f"
    let result = dudePredicate testRecord
    Assert.True(result)

[<Fact>] 
let ``predicate language should support quoted values, even when they contain a .`` () =
    let testRecord = { Name = "Dude.Duderson"; Sex = 'f' }
    let dudePredicate = buildExpr<DudeRecord, bool> "Name = \"Dude.Duderson\" and Sex = f"
    let result = dudePredicate testRecord
    Assert.True(result)


type DudeRecordWithInt = { Name: string; Age: int }
    with member t.GetAge() = t.Age

[<Fact>] 
let ``predicate language should support equality of non-string types`` () =
    let testRecord = { Name = "Dude Duderson"; Age = 20 }
    let dudePredicate = buildExpr<DudeRecordWithInt,bool> "Name = \"Dude Duderson\" and Age = 20"
    let result = dudePredicate testRecord
    Assert.True(result)

type ParentWithObject = { State: string; Data: obj }

[<Fact>]
let ``predicate language should support dynamic property lookup on unknown types`` () =
    let childRec = { Name = "Dude Duderson"; Age = 20 }
    let parentRec = { State = "Washington"; Data = childRec :> obj }
    let dudePredicate = buildExpr<ParentWithObject,bool> "Data.Name = \"Dude Duderson\" and Data.Age < 30"
    let result = dudePredicate parentRec
    Assert.True(result)

[<Fact>]
let ``predicate language should support dynamic method lookup on unknown types`` () =
    let childRec = { Name = "Dude Duderson"; Age = 20 }
    let parentRec = { State = "Washington"; Data = childRec :> obj }
    let dudePredicate = buildExpr<ParentWithObject,bool> "Data.Name = \"Dude Duderson\" and Data.GetAge() < 30"
    let result = dudePredicate parentRec
    Assert.True(result)

[<Fact>] 
let ``predicate language should support less than`` () =
    let testRecord = { Name = "Dude Duderson"; Age = 20 }
    let dudePredicate = buildExpr<DudeRecordWithInt,bool> "Name = \"Dude Duderson\" and Age < 30"
    let result = dudePredicate testRecord
    Assert.True(result)

[<Fact>] 
let ``predicate language should support greater than`` () =
    let testRecord = { Name = "Dude Duderson"; Age = 20 }
    let dudePredicate = buildExpr<DudeRecordWithInt,bool> "Name = \"Dude Duderson\" and Age > 15"
    let result = dudePredicate testRecord
    Assert.True(result)

[<Fact>] 
let ``predicate language should support greater than or equal to`` () =
    let testRecord = { Name = "Dude Duderson"; Age = 20 }
    let dudePredicate = buildExpr<DudeRecordWithInt,bool> "Name = \"Dude Duderson\" and Age >= 20"
    let result = dudePredicate testRecord
    Assert.True(result)

[<Fact>] 
let ``predicate language should support less than or equal to`` () =
    let testRecord = { Name = "Dude Duderson"; Age = 20 }
    let dudePredicate = buildExpr<DudeRecordWithInt,bool> "Name = \"Dude Duderson\" and Age <= 20"
    let result = dudePredicate testRecord
    Assert.True(result)

[<Fact>] 
let ``predicate language should support not`` () =
    let testRecord = { Name = "Dude Duderson"; Age = 20 }
    let dudePredicate = buildExpr<DudeRecordWithInt,bool> "Name = \"Dude Duderson\" and not Age = 19"
    let result = dudePredicate testRecord
    Assert.True(result)

[<Fact>] 
let ``predicate language should support not equal`` () =
    let testRecord = { Name = "Dude Duderson"; Age = 20 }
    let dudePredicate = buildExpr<DudeRecordWithInt,bool> "Name = \"Dude Duderson\" and Age <> 19"
    let result = dudePredicate testRecord
    Assert.True(result)

[<Fact>] 
let ``predicate language should support parens`` () =
    let testRecord = { Name = "Dude Duderson"; Age = 20 }
    let dudePredicate = buildExpr<DudeRecordWithInt,bool> "Name = \"Dude Duderson\" and (Age <> 19)"
    let result = dudePredicate testRecord
    Assert.True(result)

[<Fact>] 
let ``predicate language should support parens over entire predicate`` () =
    let testRecord = { Name = "Dude Duderson"; Age = 20 }
    let dudePredicate = buildExpr<DudeRecordWithInt,bool> "(Name = \"Dude Duderson\" (and) (Age <> 19))"
    let result = dudePredicate testRecord
    Assert.True(result)

[<Fact>] 
let ``predicate language should support no argument methods`` () =
    let testRecord = { Name = " Dude Duderson "; Age = 20 }
    let dudePredicate = buildExpr<DudeRecordWithInt,bool> "Name.Trim() = \"Dude Duderson\""
    let result = dudePredicate testRecord
    Assert.True(result)

[<Fact>] 
let ``predicate language should support single argument methods`` () =
    let testRecord = { Name = "Dude Duderson"; Age = 20 }
    let dudePredicate = buildExpr<DudeRecordWithInt,bool> "Name.Contains(\"Dude Duderson\")"
    let result = dudePredicate testRecord
    Assert.True(result)

[<Fact>] 
let ``predicate language should support multi-argument methods`` () =
    let testRecord = { Name = "Dude Duderson"; Age = 20 }
    let dudePredicate = buildExpr<DudeRecordWithInt,bool> "Name.Substring(0, 4) = Dude"
    let result = dudePredicate testRecord
    Assert.True(result)

[<Fact>]
let ``predicate language should support comparing null values`` () =
    let testRecord = { Name = null; Age = 20 }
    let dudePredicate = buildExpr<DudeRecordWithInt,bool> "Name = null"
    let result = dudePredicate testRecord
    Assert.True(result)

type OptionRecord = { OptionalName: string option; Age: int }

[<Fact>]
let ``predicate language shouldn't choke on option types`` () =
    let testRecord = { OptionalName = Some "Dude"; Age = 20 }
    let dudePredicate = buildExpr<OptionRecord,bool> "OptionalName = Dude"
    let result = dudePredicate testRecord
    Assert.True(result)    