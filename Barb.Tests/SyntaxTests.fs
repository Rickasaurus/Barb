module PredicateLanguage

open Barb.Compiler

open Xunit

type DudeRecord = { Name: string; Sex: char }

[<Fact>] 
let ``predicate language should work with a simple predicate`` () =
    let testRecord = { Name = "Dude"; Sex = 'm' }
    let predicate = buildExpr<DudeRecord,bool> "Name = \"Dude\""
    let result = predicate testRecord
    Assert.True(result)

[<Fact>] 
let ``predicate language should work with double equals`` () =
    let testRecord = { Name = "Dude"; Sex = 'm' }
    let predicate = buildExpr<DudeRecord,bool> "Name == \"Dude\""
    let result = predicate testRecord
    Assert.True(result)

[<Fact>] 
let ``predicate language should ignore whitespace, even at the end of a token`` () =
    let testRecord = { Name = "Dude"; Sex = 'f' }
    let predicate = buildExpr<DudeRecord,bool> " true "
    let result = predicate testRecord
    Assert.True(result)

[<Fact>] 
let ``predicate language should support simple and usage`` () =
    let testRecord = { Name = "Dude"; Sex = 'f' }
    let predicate = buildExpr<DudeRecord,bool> "true and true"
    let result = predicate testRecord
    Assert.True(result)

[<Fact>] 
let ``predicate language should work with a compound predicate`` () =
    let testRecord = { Name = "Dude"; Sex = 'f' }
    let predicate = buildExpr<DudeRecord,bool> "Name = \"Dude\" and Sex = 'f'"
    let result = predicate testRecord
    Assert.True(result)

type DudeRecordWithInt = { Name: string; Age: int }
    with member t.GetAge() = t.Age

[<Fact>] 
let ``predicate language should support equality of non-string types`` () =
    let testRecord = { Name = "Dude Duderson"; Age = 20 }
    let predicate = buildExpr<DudeRecordWithInt,bool> "Name = \"Dude Duderson\" and Age = 20"
    let result = predicate testRecord
    Assert.True(result)

[<Fact>] 
let ``predicate language should support less than`` () =
    let testRecord = { Name = "Dude Duderson"; Age = 20 }
    let predicate = buildExpr<DudeRecordWithInt,bool> "Name = \"Dude Duderson\" and Age < 30"
    let result = predicate testRecord
    Assert.True(result)

[<Fact>] 
let ``predicate language should support greater than`` () =
    let testRecord = { Name = "Dude Duderson"; Age = 20 }
    let predicate = buildExpr<DudeRecordWithInt,bool> "Name = \"Dude Duderson\" and Age > 15"
    let result = predicate testRecord
    Assert.True(result)

[<Fact>] 
let ``predicate language should support greater than or equal to`` () =
    let testRecord = { Name = "Dude Duderson"; Age = 20 }
    let predicate = buildExpr<DudeRecordWithInt,bool> "Name = \"Dude Duderson\" and Age >= 20"
    let result = predicate testRecord
    Assert.True(result)

[<Fact>] 
let ``predicate language should support less than or equal to`` () =
    let testRecord = { Name = "Dude Duderson"; Age = 20 }
    let predicate = buildExpr<DudeRecordWithInt,bool> "Name = \"Dude Duderson\" and Age <= 20"
    let result = predicate testRecord
    Assert.True(result)

[<Fact>] 
let ``predicate language should support not`` () =
    let testRecord = { Name = "Dude Duderson"; Age = 20 }
    let predicate = buildExpr<DudeRecordWithInt,bool> "Name = \"Dude Duderson\" and not (Age = 19)"
    let result = predicate testRecord
    Assert.True(result)

[<Fact>] 
let ``predicate language should support not equal`` () =
    let testRecord = { Name = "Dude Duderson"; Age = 20 }
    let predicate = buildExpr<DudeRecordWithInt,bool> "Name = \"Dude Duderson\" and Age <> 19"
    let result = predicate testRecord
    Assert.True(result)

[<Fact>] 
let ``predicate language should support parens`` () =
    let testRecord = { Name = "Dude Duderson"; Age = 20 }
    let predicate = buildExpr<DudeRecordWithInt,bool> "Name = \"Dude Duderson\" and (Age <> 19)"
    let result = predicate testRecord
    Assert.True(result)

[<Fact>] 
let ``predicate language should support parens over entire predicate`` () =
    let testRecord = { Name = "Dude Duderson"; Age = 20 }
    let predicate = buildExpr<DudeRecordWithInt,bool> "(Name = \"Dude Duderson\" (and) (Age <> 19))"
    let result = predicate testRecord
    Assert.True(result)


[<Fact>]
let ``predicate language should support comparing null values`` () =
    let testRecord = { Name = null; Age = 20 }
    let predicate = buildExpr<DudeRecordWithInt,bool> "Name = null"
    let result = predicate testRecord
    Assert.True(result)

[<Fact>]
let ``predicate language should support object indexers`` () =
    let testRecord = { Name = "Dude Duderson"; Age = 20 }
    let predicate = buildExpr<DudeRecordWithInt,bool> "Name[0] = 'D'"
    let result = predicate testRecord
    Assert.True(result)

[<Fact>]
let ``predicate language should support F#-like indexers`` () = 
    let testRecord = { Name = "Dude Duderson"; Age = 20 }
    let predicate = buildExpr<DudeRecordWithInt,bool> "Name.[0] = 'D'"
    let result = predicate testRecord
    Assert.True(result)

type BoolRec = { HasHat: bool; Name: string }

type TestFloatRec = { Score: float }

[<Fact>]
let ``predicate language should properly parse floating point numbers`` () = 
    let testRecord = { Score = 0.90 }
    let predicate = buildExpr<TestFloatRec,bool> "Score < 1.0"
    let result = predicate testRecord
    Assert.True(result)    

[<Fact>]
let ``predicate language should allow simple variable binding with let`` () = 
    let testRecord = { HasHat = true; Name = "Don" }
    let predicate = buildExpr<BoolRec,bool> "let x = true in x = HasHat"
    let result = predicate testRecord
    Assert.True(result)    

[<Fact>]
let ``predicate language should allow simple variable binding with var`` () = 
    let testRecord = { HasHat = true; Name = "Don" }
    let predicate = buildExpr<BoolRec,bool> "var x = true in x = HasHat"
    let result = predicate testRecord
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
    let predicate = buildExpr<TestListRec<string>,bool> "Nums = (\"one\",\"two\",\"three\")"
    let result = predicate testRecord
    Assert.True(result)    

[<Fact>] 
let ``predicate language tuples should compare correctly with value-type lists`` () = 
    let testRecord = { Nums = [1;2;3;4;5] }
    let predicate = buildExpr<TestListRec<int>,bool> "Nums = (1,2,3,4,5)"
    let result = predicate testRecord
    Assert.True(result)    

[<Fact>]
let ``predicate language should support internal use of lambdas`` () = 
    let testRecord = { HasHat = false; Name = "Don" }
    let predicate = buildExpr<BoolRec,bool> "let fx = (fun x -> x = \"Don\") in fx Name"
    let result = predicate testRecord
    Assert.True(result) 

[<Fact>]
let ``predicate language should support functions with multiple parameters`` () =
    let predicate = buildExpr<unit, bool> "let f = fun x y -> x + y in f 1 2 = 3"
    let result = predicate ()
    Assert.True(result)  

[<Fact>]
let ``predicate language should support internal use of lambdas with more than one parameter`` () = 
    let testRecord = { HasHat = false; Name = "Don" }
    let predicate = buildExpr<BoolRec,bool> "let fx = (fun x y -> x = \"Don\" and not y) in fx Name HasHat"
    let result = predicate testRecord
    Assert.True(result) 

[<Fact>]
let ``predicate language lambda parameters should scope external bindings`` () = 
    let testRecord = { HasHat = false; Name = "Don" }
    let predicate = buildExpr<BoolRec,bool> "let x = 1 in let fx = (fun x -> x = \"Don\") in fx Name"
    let result = predicate testRecord
    Assert.True(result) 


[<Fact>]
let ``predicate language should support int addition`` () = 
    let predicate = buildExpr<unit,bool> "1 + 2 = 3"
    let result = predicate ()
    Assert.True(result) 

[<Fact>]
let ``predicate language should support int subtraction`` () = 
    let predicate = buildExpr<unit,bool> "2 - 2 = 0"
    let result = predicate ()
    Assert.True(result)
    
[<Fact>]
let ``predicate language should support int division`` () = 
    let predicate = buildExpr<unit,bool> "4 / 2 = 2"
    let result = predicate ()
    Assert.True(result) 

[<Fact>]
let ``predicate language should support int multiplication`` () = 
    let predicate = buildExpr<unit,bool> "2 * 2 = 4"
    let result = predicate ()
    Assert.True(result)  


[<Fact>]
let ``predicate language should support decimal addition`` () = 
    let predicate = buildExpr<unit,bool> "1.0 + 2.0 = 3.0"
    let result = predicate ()
    Assert.True(result) 

[<Fact>]
let ``predicate language should support decimal subtraction`` () = 
    let predicate = buildExpr<unit,bool> "2.0 - 2.0 = 0"
    let result = predicate ()
    Assert.True(result)
    
[<Fact>]
let ``predicate language should support decimal div.0ision`` () = 
    let predicate = buildExpr<unit,bool> "4 / 2.0 = 2.0"
    let result = predicate ()
    Assert.True(result) 

[<Fact>]
let ``predicate language should support decimal multiplication`` () = 
    let predicate = buildExpr<unit,bool> "2.0 * 2.0 = 4.0"
    let result = predicate ()
    Assert.True(result)  


[<Fact>]
let ``predicate language should support if-then-else`` () = 
    let testRec = { Score = 101.0 } 
    let predicate = buildExpr<TestFloatRec,bool> "(if Score >= 100 then 5 else 10) = 5"
    let result = predicate testRec
    Assert.True(result)  

[<Fact>]
let ``predicate language should support if-then-else without an explicit subexpression`` () = 
    let testRec = { Score = 101.0 } 
    let predicate = buildExpr<TestFloatRec,bool> "if Score >= 100 then true else false"
    let result = predicate testRec
    Assert.True(result)  


[<Fact>]
let ``predicate language should support if-then-else with initial/final spacing`` () = 
    let testRec = { Score = 101.0 } 
    let predicate = buildExpr<TestFloatRec,bool> "( if Score >= 100 then 5 else 10 ) = 5"
    let result = predicate testRec
    Assert.True(result)  

[<Fact>]
let ``predicate language should support iterative incremental tuple building`` () = 
    let predicate = buildExpr<unit,bool> "{1 .. 5} = (1, 2, 3, 4, 5)"
    let result = predicate ()
    Assert.True(result)  

[<Fact>]
let ``predicate language should support iterative incremental tuple building with jumps`` () = 
    let predicate = buildExpr<unit,bool> "{0 .. 2 .. 10} = (0, 2, 4, 6, 8, 10)"
    let result = predicate ()
    Assert.True(result)  

[<Fact>]
let ``predicate language should treat tuples properly even when in a left subexpression`` () = 
    let predicate = buildExpr<unit,bool> "((1,2,3)) = (1,2,3)"
    let result = predicate ()
    Assert.True(result)  

[<Fact>]
let ``predicate language should treat tuples properly even when in a right subexpression`` () = 
    let predicate = buildExpr<unit,bool> "(1,2,3) = ((1,2,3))"
    let result = predicate ()
    Assert.True(result)  

//let fx = (fun x -> x = \"Don\") in fx Name"

[<Fact>]
let ``predicate language should support recursion`` () = 
    let predicate = buildExpr<unit,bool> "let fx = (fun x -> (if x = 0 then true else fx (x - 1))) in fx 5"
    let result = predicate ()
    Assert.True(result)  

[<Fact>]
let ``predicate language tuples should be indexable`` () = 
    let predicate = buildExpr<unit,bool> "(1,2,3)[1] = 2"
    let result = predicate ()
    Assert.True(result)  

[<Fact>]
let ``predicate language should support safe while syntax`` () = 
    let predicate = buildExpr<unit,bool> "let x = 1 in { while x < 5 do x = x + 1 } x = 4"
    let result = predicate ()
    Assert.True(result)  

//[<Fact>]
//let ``predicate language should support a simple fold`` () = 
//    let predicate = buildExpr<unit,bool> "(fold (1, 2, 3) with true in (fun e s -> s and e < 5))"
//    let result = predicate ()
//    Assert.True(result)  

