module PredicateLanguage

open Barb.Compiler

open Xunit

type DudeRecord = { Name: string; Sex: char }

[<Fact>] 
let ``predicate language should work with a simple predicate`` () =
    let testRecord = { Name = "Dude"; Sex = 'm' }
    let dudePredicate = buildExpr<DudeRecord,bool> "Name = \"Dude\""
    let result = dudePredicate testRecord
    Assert.True(result)

[<Fact>] 
let ``predicate language should work with a compound predicate`` () =
    let testRecord = { Name = "Dude"; Sex = 'f' }
    let dudePredicate = buildExpr<DudeRecord,bool> "Name = \"Dude\" and Sex = 'f'"
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
    let dudePredicate = buildExpr<DudeRecordWithInt,bool> "Name = \"Dude Duderson\" and not (Age = 19)"
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
let ``predicate language should support comparing null values`` () =
    let testRecord = { Name = null; Age = 20 }
    let dudePredicate = buildExpr<DudeRecordWithInt,bool> "Name = null"
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

type BoolRec = { HasHat: bool; Name: string }

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
    Assert.Throws(typeof<System.Exception>, new Assert.ThrowsDelegate(fun () -> 
        let func = buildExpr<BoolRec,bool> "(let x = true in x = HasHat) && x = true"
        let res = func(testRecord)
        res |> ignore))

type TestListRec<'T> = { Nums: 'T list }

[<Fact>] 
let ``predicate language tuples should compare correctly with reference type lists`` () = 
    let testRecord = { Nums = ["one";"two";"three"] }
    let dudePredicate = buildExpr<TestListRec<string>,bool> "Nums = (\"one\",\"two\",\"three\")"
    let result = dudePredicate testRecord
    Assert.True(result)    

[<Fact>] 
let ``predicate language tuples should compare correctly with value-type lists`` () = 
    let testRecord = { Nums = [1;2;3;4;5] }
    let dudePredicate = buildExpr<TestListRec<int>,bool> "Nums = (1,2,3,4,5)"
    let result = dudePredicate testRecord
    Assert.True(result)    

[<Fact>]
let ``predicate language should support internal use of lambdas`` () = 
    let testRecord = { HasHat = false; Name = "Don" }
    let dudePredicate = buildExpr<BoolRec,bool> "let fx = (fun x -> x = \"Don\") in fx Name"
    let result = dudePredicate testRecord
    Assert.True(result) 

[<Fact>]
let ``predicate language should support int addition`` () = 
    let dudePredicate = buildExpr<unit,bool> "1 + 2 = 3"
    let result = dudePredicate ()
    Assert.True(result) 

[<Fact>]
let ``predicate language should support int subtraction`` () = 
    let dudePredicate = buildExpr<unit,bool> "2 - 2 = 0"
    let result = dudePredicate ()
    Assert.True(result)
    
[<Fact>]
let ``predicate language should support int division`` () = 
    let dudePredicate = buildExpr<unit,bool> "4 / 2 = 2"
    let result = dudePredicate ()
    Assert.True(result) 

[<Fact>]
let ``predicate language should support int multiplication`` () = 
    let dudePredicate = buildExpr<unit,bool> "2 * 2 = 4"
    let result = dudePredicate ()
    Assert.True(result)  


[<Fact>]
let ``predicate language should support decimal addition`` () = 
    let dudePredicate = buildExpr<unit,bool> "1.0 + 2.0 = 3.0"
    let result = dudePredicate ()
    Assert.True(result) 

[<Fact>]
let ``predicate language should support decimal subtraction`` () = 
    let dudePredicate = buildExpr<unit,bool> "2.0 - 2.0 = 0"
    let result = dudePredicate ()
    Assert.True(result)
    
[<Fact>]
let ``predicate language should support decimal division`` () = 
    let dudePredicate = buildExpr<unit,bool> "4.0 / 2.0 = 2.0"
    let result = dudePredicate ()
    Assert.True(result) 

[<Fact>]
let ``predicate language should support decimal multiplication`` () = 
    let dudePredicate = buildExpr<unit,bool> "2.0 * 2.0 = 4.0"
    let result = dudePredicate ()
    Assert.True(result)  

