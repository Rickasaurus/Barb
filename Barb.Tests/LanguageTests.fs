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

[<Fact>] 
let ``predicate language should support escaped quotes`` () =
    let testRecord = { Name = "Dude\"Duderson"; Sex = 'f' }
    let dudePredicate = buildExpr<DudeRecord,bool> "Name = \"Dude\\\"Duderson\" and Sex = f"
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

[<Fact>]
let ``predicate language should support invoking a method on the results of a method call`` () =
    let testRecord = { Name = "Dude Duderson"; Age = 20 }
    let dudePredicate = buildExpr<DudeRecordWithInt,bool> "Name.Substring(0, 4).Length = 4"
    let result = dudePredicate testRecord
    Assert.True(result)

[<Fact>]
let ``predicate language should support invoking a method on the results of a subexpression`` () =
    let testRecord = { Name = "Dude Duderson"; Age = 20 }
    let dudePredicate = buildExpr<DudeRecordWithInt,bool> "(Name.Substring(0, 4)).Length = 4"
    let result = dudePredicate testRecord
    Assert.True(result)

[<Fact>]
let ``predicate language should support object indexers`` () =
    let testRecord = { Name = "Dude Duderson"; Age = 20 }
    let dudePredicate = buildExpr<DudeRecordWithInt,bool> "Name[0] = 'D'"
    let result = dudePredicate testRecord
    Assert.True(result)

[<Fact>]
let ``predicate language should support F#-like indexers`` () = 
    let testRecord = { Name = "Dude Duderson"; Age = 20 }
    let dudePredicate = buildExpr<DudeRecordWithInt,bool> "Name.[0] = 'D'"
    let result = dudePredicate testRecord
    Assert.True(result)

type PropIndexerTester<'a,'b when 'a : comparison> (map: Map<'a,'b>) = 
    member this.Item
        with get(indexer: 'a) : 'b = map |> Map.find indexer

type IndexerRecord<'a,'b when 'a : comparison> = 
    {
        Name: string
        Table: PropIndexerTester<'a,'b>
    }

[<Fact>]
let ``predicate language should support property indexers`` () = 
    let testRecord = { Name = "Dude Duderson"; Table = new PropIndexerTester<int,int>([0..5] |> List.map (fun i -> i, i) |> Map.ofList) }
    let dudePredicate = buildExpr<IndexerRecord<int,int>,bool> "Table.Item[0] = 0"
    let result = dudePredicate testRecord
    Assert.True(result)    

[<Fact>]
let ``predicate language should support property indexers with strings`` () = 
    let testRecord = { Name = "Dude Duderson"; Table = new PropIndexerTester<string,string>(["one"; "two"; "three"] |> List.map (fun i -> i, i) |> Map.ofList) }
    let dudePredicate = buildExpr<IndexerRecord<string,string>,bool> "Table.Item[\"two\"] = \"two\""
    let result = dudePredicate testRecord
    Assert.True(result)    

type BoolRec = { HasHat: bool; Name: string }

[<Fact>]
let ``predicate language should preserve left-to-right order of operations with record bool`` () = 
    let testRecord = { HasHat = true; Name = "Howard" }
    let dudePredicate = buildExpr<BoolRec,bool> "HasHat and Name = Howard"
    let result = dudePredicate testRecord
    Assert.True(result)    

[<Fact>]
let ``predicate language should preserve left-to-right order of operations with explicit bool`` () = 
    let testRecord = { HasHat = false; Name = "Don" }
    let dudePredicate = buildExpr<BoolRec,bool> "HasHat = false and Name = Don"
    let result = dudePredicate testRecord
    Assert.True(result)    

[<Fact>]
let ``predicate language should evalute order of boolean ops correctly`` () = 
    let testRecord = { HasHat = true; Name = "Don" }
    let dudePredicate = buildExpr<BoolRec,bool> "true and HasHat or false"
    let result = dudePredicate testRecord
    Assert.True(result)    

type TestFloatRec = { Score: float }

[<Fact>]
let ``predicate language should properly parse floating point numbers`` () = 
    let testRecord = { Score = 0.90 }
    let dudePredicate = buildExpr<TestFloatRec,bool> "Score < 1.0"
    let result = dudePredicate testRecord
    Assert.True(result)    

[<Fact>]
let ``predicate language should allow simple variable binding with let`` () = 
    let testRecord = { HasHat = true; Name = "Don" }
    let dudePredicate = buildExpr<BoolRec,bool> "let x = true in x = HasHat"
    let result = dudePredicate testRecord
    Assert.True(result)    

[<Fact>]
let ``predicate language should allow simple variable binding with var`` () = 
    let testRecord = { HasHat = true; Name = "Don" }
    let dudePredicate = buildExpr<BoolRec,bool> "var x = true in x = HasHat"
    let result = dudePredicate testRecord
    Assert.True(result)    

[<Fact>]
let ``predicate language should follow scoping rules for bound variables`` () = 
    let testRecord = { HasHat = true; Name = "Don" }
    Assert.Throws(typeof<System.Exception>, new Assert.ThrowsDelegate(fun () -> buildExpr<BoolRec,bool> "(let x = true in x = HasHat) && x = true" |> ignore))

type TestArrayRec<'T> = { Nums: 'T list }

[<Fact>] 
let ``predicate language tuples should compare correctly with reference type lists`` () = 
    let testRecord = { Nums = ["one";"two";"three"] }
    let dudePredicate = buildExpr<TestArrayRec<string>,bool> "Nums = (\"one\",\"two\",\"three\")"
    let result = dudePredicate testRecord
    Assert.True(result)    

[<Fact>] 
let ``predicate language tuples should compare correctly with value-type lists`` () = 
    let testRecord = { Nums = [1;2;3;4;5] }
    let dudePredicate = buildExpr<TestArrayRec<int>,bool> "Nums = (1,2,3,4,5)"
    let result = dudePredicate testRecord
    Assert.True(result)    

//[<Fact>]
//let ``predicate language should support internal use of lambdas`` () = 
//    let testRecord = { HasHat = true; Name = "Don" }
//    let dudePredicate = buildExpr<BoolRec,bool> "let x = true in x = HasHat"
//    let result = dudePredicate testRecord
//    Assert.True(result) 