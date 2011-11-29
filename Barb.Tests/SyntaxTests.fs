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
let ``predicate language should work with double equals`` () =
    let testRecord = { Name = "Dude"; Sex = 'm' }
    let dudePredicate = buildExpr<DudeRecord,bool> "Name == \"Dude\""
    let result = dudePredicate testRecord
    Assert.True(result)

[<Fact>] 
let ``predicate language should ignore whitespace, even at the end of a token`` () =
    let testRecord = { Name = "Dude"; Sex = 'f' }
    let dudePredicate = buildExpr<DudeRecord,bool> " true "
    let result = dudePredicate testRecord
    Assert.True(result)

[<Fact>] 
let ``predicate language should support simple and usage`` () =
    let testRecord = { Name = "Dude"; Sex = 'f' }
    let dudePredicate = buildExpr<DudeRecord,bool> "true and true"
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
let ``predicate language should support internal use of lambdas with more than one parameter`` () = 
    let testRecord = { HasHat = false; Name = "Don" }
    let dudePredicate = buildExpr<BoolRec,bool> "let fx = (fun x y -> x = \"Don\" and not y) in fx Name HasHat"
    let result = dudePredicate testRecord
    Assert.True(result) 

[<Fact>]
let ``predicate language lambda parameters should scope external bindings`` () = 
    let testRecord = { HasHat = false; Name = "Don" }
    let dudePredicate = buildExpr<BoolRec,bool> "let x = 1 in let fx = (fun x -> x = \"Don\") in fx Name"
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
let ``predicate language should support decimal div.0ision`` () = 
    let dudePredicate = buildExpr<unit,bool> "4 / 2.0 = 2.0"
    let result = dudePredicate ()
    Assert.True(result) 

[<Fact>]
let ``predicate language should support decimal multiplication`` () = 
    let dudePredicate = buildExpr<unit,bool> "2.0 * 2.0 = 4.0"
    let result = dudePredicate ()
    Assert.True(result)  


[<Fact>]
let ``predicate language should support if-then-else`` () = 
    let testRec = { Score = 101.0 } 
    let dudePredicate = buildExpr<TestFloatRec,bool> "(if Score >= 100 then 5 else 10) = 5"
    let result = dudePredicate testRec
    Assert.True(result)  

[<Fact>]
let ``predicate language should support if-then-else with initial/final spacing`` () = 
    let testRec = { Score = 101.0 } 
    let dudePredicate = buildExpr<TestFloatRec,bool> "( if Score >= 100 then 5 else 10 ) = 5"
    let result = dudePredicate testRec
    Assert.True(result)  

[<Fact>]
let ``predicate language should support iterative incremental tuple building`` () = 
    let dudePredicate = buildExpr<unit,bool> "{1 .. 5} = (1, 2, 3, 4, 5)"
    let result = dudePredicate ()
    Assert.True(result)  

[<Fact>]
let ``predicate language should support iterative incremental tuple building with jumps`` () = 
    let dudePredicate = buildExpr<unit,bool> "{0 .. 2 .. 10} = (0, 2, 4, 6, 8, 10)"
    let result = dudePredicate ()
    Assert.True(result)  

[<Fact>]
let ``predicate language should treat tuples properly even when in a left subexpression`` () = 
    let dudePredicate = buildExpr<unit,bool> "((1,2,3)) = (1,2,3)"
    let result = dudePredicate ()
    Assert.True(result)  

[<Fact>]
let ``predicate language should treat tuples properly even when in a right subexpression`` () = 
    let dudePredicate = buildExpr<unit,bool> "(1,2,3) = ((1,2,3))"
    let result = dudePredicate ()
    Assert.True(result)  

//let fx = (fun x -> x = \"Don\") in fx Name"

[<Fact>]
let ``predicate language should support recursion`` () = 
    let dudePredicate = buildExpr<unit,bool> "let fx = (fun x -> (if x = 0 then true else fx (x - 1))) in fx 5"
    let result = dudePredicate ()
    Assert.True(result)  

[<Fact>]
let ``predicate language tuples should be indexable`` () = 
    let dudePredicate = buildExpr<unit,bool> "(1,2,3)[1] = 2"
    let result = dudePredicate ()
    Assert.True(result)  

//[<Fact>]
//let ``predicate language should support a simple fold`` () = 
//    let dudePredicate = buildExpr<unit,bool> "(fold (1, 2, 3) with true in (fun e s -> s and e < 5))"
//    let result = dudePredicate ()
//    Assert.True(result)  

