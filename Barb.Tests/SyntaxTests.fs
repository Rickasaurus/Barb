module Barb.Tests.PredicateLanguage

open Barb.Compiler

open Xunit

type DudeRecord = { Name: string; Sex: char }

[<Fact>] 
let ``should work with a simple predicate`` () =
    let testRecord = { Name = "Dude"; Sex = 'm' }
    let predicate = buildExpr<DudeRecord,bool> "Name = \"Dude\""
    let result = predicate testRecord
    Assert.True(result)

[<Fact>] 
let ``should work with double equals`` () =
    let testRecord = { Name = "Dude"; Sex = 'm' }
    let predicate = buildExpr<DudeRecord,bool> "Name == \"Dude\""
    let result = predicate testRecord
    Assert.True(result)

[<Fact>] 
let ``should ignore whitespace, even at the end of a token`` () =
    let testRecord = { Name = "Dude"; Sex = 'f' }
    let predicate = buildExpr<DudeRecord,bool> " true "
    let result = predicate testRecord
    Assert.True(result)

[<Fact>] 
let ``should support simple and usage`` () =
    let testRecord = { Name = "Dude"; Sex = 'f' }
    let cases = 
        [ "true and true", true
          "true and false", false
          "false and true", false
          "false and false", false ]
    for expr, res in cases do        
        let predicate = buildExpr<DudeRecord,bool> expr
        let result = predicate testRecord
        do Assert.Equal(result, res)

[<Fact>] 
let ``should support simple or usage`` () =
    let testRecord = { Name = "Dude"; Sex = 'f' }
    let cases = 
        [ "true or true", true
          "true or false", true
          "false or true", true
          "false or false", false 
          ]
    for expr, res in cases do        
        let predicate = buildExpr<DudeRecord,bool> expr
        let result = predicate testRecord
        do Assert.Equal(result, res)

type TFRec = { FTrue: bool; FFalse: bool }
let TFRecInstance = { FTrue = true; FFalse = false }

[<Fact>] 
let ``should support record and usage`` () =
    let cases = 
        [ "FTrue and FTrue", true
          "FTrue and FFalse", false
          "FFalse and FTrue", false
          "FFalse and FFalse", false ]
    for expr, res in cases do        
        let predicate = buildExpr<TFRec,bool> expr
        let result = predicate TFRecInstance
        do Assert.Equal(result, res)

[<Fact>] 
let ``should support record or usage`` () =
    let cases = 
        [ "FTrue or FTrue", true
          "FTrue or FFalse", true
          "FFalse or FTrue", true
          "FFalse or FFalse", false 
          ]
    for expr, res in cases do        
        let predicate = buildExpr<TFRec,bool> expr
        let result = predicate TFRecInstance
        do Assert.Equal(result, res)


[<Fact>] 
let ``should work with a compound predicate`` () =
    let testRecord = { Name = "Dude"; Sex = 'f' }
    let predicate = buildExpr<DudeRecord,bool> "Name = \"Dude\" and Sex = 'f'"
    let result = predicate testRecord
    Assert.True(result)

type DudeRecordWithInt = { Name: string; Age: int }
    with member t.GetAge() = t.Age

[<Fact>] 
let ``should support equality of non-string types`` () =
    let testRecord = { Name = "Dude Duderson"; Age = 20 }
    let predicate = buildExpr<DudeRecordWithInt,bool> "Name = \"Dude Duderson\" and Age = 20"
    let result = predicate testRecord
    Assert.True(result)

[<Fact>] 
let ``should support less than`` () =
    let testRecord = { Name = "Dude Duderson"; Age = 20 }
    let predicate = buildExpr<DudeRecordWithInt,bool> "Name = \"Dude Duderson\" and Age < 30"
    let result = predicate testRecord
    Assert.True(result)

[<Fact>] 
let ``should support greater than`` () =
    let testRecord = { Name = "Dude Duderson"; Age = 20 }
    let predicate = buildExpr<DudeRecordWithInt,bool> "Name = \"Dude Duderson\" and Age > 15"
    let result = predicate testRecord
    Assert.True(result)

[<Fact>] 
let ``should support greater than or equal to`` () =
    let testRecord = { Name = "Dude Duderson"; Age = 20 }
    let predicate = buildExpr<DudeRecordWithInt,bool> "Name = \"Dude Duderson\" and Age >= 20"
    let result = predicate testRecord
    Assert.True(result)

[<Fact>] 
let ``should support less than or equal to`` () =
    let testRecord = { Name = "Dude Duderson"; Age = 20 }
    let predicate = buildExpr<DudeRecordWithInt,bool> "Name = \"Dude Duderson\" and Age <= 20"
    let result = predicate testRecord
    Assert.True(result)

[<Fact>] 
let ``should support not`` () =
    let testRecord = { Name = "Dude Duderson"; Age = 20 }
    let predicate = buildExpr<DudeRecordWithInt,bool> "Name = \"Dude Duderson\" and not (Age = 19)"
    let result = predicate testRecord
    Assert.True(result)

[<Fact>] 
let ``should support not equal`` () =
    let testRecord = { Name = "Dude Duderson"; Age = 20 }
    let predicate = buildExpr<DudeRecordWithInt,bool> "Name = \"Dude Duderson\" and Age <> 19"
    let result = predicate testRecord
    Assert.True(result)

[<Fact>] 
let ``should support parens`` () =
    let testRecord = { Name = "Dude Duderson"; Age = 20 }
    let predicate = buildExpr<DudeRecordWithInt,bool> "Name = \"Dude Duderson\" and (Age <> 19)"
    let result = predicate testRecord
    Assert.True(result)

[<Fact>]
let ``should support comparing null values`` () =
    let testRecord = { Name = null; Age = 20 }
    let predicate = buildExpr<DudeRecordWithInt,bool> "Name = null"
    let result = predicate testRecord
    Assert.True(result)

[<Fact>]
let ``most infix numeric operators on null should return null`` () =
    let opers = [|"+"; "-"; "/"; "*"; "&"; "|"; "|||"; "&&&" |]
    for op in opers do
        let predicate = buildExpr<unit,bool> ("(10 " + op + " null) = null")
        let res = predicate()
        Assert.True(res, "null right failed on: " + op + " with " + res.ToString())    
        let predicate = buildExpr<unit,bool> ("(null " + op + " 10) = null")
        let res = predicate()
        Assert.True(res, "null left failed on: " + op + " with " + res.ToString())    

[<Fact>]
let ``and operators on null should return null`` () =
    let opers = [|"&&"; "and"|]
    for op in opers do
        let predicate = buildExpr<unit,bool> ("(true " + op + " null) = null")
        let res = predicate()
        Assert.True(res, "null right failed on: " + op + " with " + res.ToString())    
        let predicate = buildExpr<unit,bool> ("(null " + op + " true) = null")
        let res = predicate()
        Assert.True(res, "null right failed on: " + op + " with " + res.ToString())    

[<Fact>]
let ``or operators on null should return null`` () =
    let opers = [|"||"; "or"|]
    for op in opers do
        let predicate = buildExpr<unit,bool> ("(false " + op + " null) = null")
        let res = predicate()
        Assert.True(res, "null right failed on: " + op + " with " + res.ToString())    
        let predicate = buildExpr<unit,bool> ("(null " + op + " false) = null")
        let res = predicate()
        Assert.True(res, "null right failed on: " + op + " with " + res.ToString())    

[<Fact>]
let ``set operators should have correct behavior with nulls`` () =
    let opsAns = [| "\\/", false; "/\\", true; "/?\\", false|]
    for op, ans in opsAns do
        let predicate = buildExpr<unit,bool> ("(10 " + op + " null) = null")
        let res = predicate()
        Assert.True((ans = res), "failed on: " + op + " with " + res.ToString())    

[<Fact>]
let ``equality and comparison operators should return true or false for nulls`` () =
    let opsAns = [|"=", false; "==", false; "<>", true; "!=", true; ">=", false; "<=", false; ">", false; "<", false|]
    for op, ans in opsAns do
        let predicate = buildExpr<unit,bool> ("10 " + op + " null")
        let res = predicate()
        Assert.Equal(ans, res)    


[<Fact>]
let ``should support object indexers`` () =
    let testRecord = { Name = "Dude Duderson"; Age = 20 }
    let predicate = buildExpr<DudeRecordWithInt,bool> "Name[0] = 'D'"
    let result = predicate testRecord
    Assert.True(result)

[<Fact>]
let ``should support F#-like indexers`` () = 
    let testRecord = { Name = "Dude Duderson"; Age = 20 }
    let predicate = buildExpr<DudeRecordWithInt,bool> "Name.[0] = 'D'"
    let result = predicate testRecord
    Assert.True(result)

type BoolRec = { HasHat: bool; Name: string }

type TestFloatRec = { Score: float }

[<Fact>]
let ``should properly parse floating point numbers`` () = 
    let testRecord = { Score = 0.90 }
    let predicate = buildExpr<TestFloatRec,bool> "Score < 1.0"
    let result = predicate testRecord
    Assert.True(result)    

[<Fact>]
let ``should properly parse floating point numbers without a leading zero`` () = 
    let testRecord = { Score = 0.90 }
    let predicate = buildExpr<TestFloatRec,bool> "Score < .95"
    let result = predicate testRecord
    Assert.True(result)    

[<Fact>]
let ``should allow simple variable binding with let`` () = 
    let testRecord = { HasHat = true; Name = "Don" }
    let predicate = buildExpr<BoolRec,bool> "let x = true in x = HasHat"
    let result = predicate testRecord
    Assert.True(result)    

[<Fact>]
let ``should allow simple variable binding with var`` () = 
    let testRecord = { HasHat = true; Name = "Don" }
    let predicate = buildExpr<BoolRec,bool> "var x = true in x = HasHat"
    let result = predicate testRecord
    Assert.True(result)    

[<Fact>]
let ``should follow scoping rules for bound variables`` () = 
    let testRecord = { HasHat = true; Name = "Don" }
    Assert.Throws(typeof<Barb.Reduce.BarbExecutionException>, new Assert.ThrowsDelegate(fun () -> 
        let func = buildExpr<BoolRec,bool> "(let x = true in x = HasHat) && x = true"
        let res = func(testRecord)
        res |> ignore))

type TestListRec<'T> = { Nums: 'T list }

[<Fact>] 
let ``tuples should compare correctly with reference type lists`` () = 
    let testRecord = { Nums = ["one";"two";"three"] }
    let predicate = buildExpr<TestListRec<string>,bool> "Nums = (\"one\",\"two\",\"three\")"
    let result = predicate testRecord
    Assert.True(result)    

[<Fact>] 
let ``tuples should compare correctly with value-type lists`` () = 
    let testRecord = { Nums = [1;2;3;4;5] }
    let predicate = buildExpr<TestListRec<int>,bool> "Nums = (1,2,3,4,5)"
    let result = predicate testRecord
    Assert.True(result)    

[<Fact>] 
let ``bound tuples should compare correctly with value-type lists`` () = 
    let testRecord = { Nums = [1;2;3;4;5] }
    let predicate = buildExpr<TestListRec<int>,bool> "let x = (1,2,3,4,5) in x = Nums"
    let result = predicate testRecord
    Assert.True(result)    

[<Fact>]
let ``should support very simple lambdas`` () =
    let predicate = buildExpr<unit,int> "let fx = fun x -> x + 1 in fx 2"
    let result = predicate ()
    Assert.Equal (3, result)

[<Fact>]
let ``should support internal use of lambdas`` () = 
    let testRecord = { HasHat = false; Name = "Don" }
    let predicate = buildExpr<BoolRec,bool> "let fx = (fun x -> x = \"Don\") in fx Name"
    let result = predicate testRecord
    Assert.True(result) 

[<Fact>]
let ``should support functions with multiple parameters`` () =
    let predicate = buildExpr<unit, bool> "let f = fun x y -> x + y in f 1 2 = 3"
    let result = predicate ()
    Assert.True(result)  

[<Fact>]
let ``should support internal use of lambdas with more than one parameter`` () = 
    let testRecord = { HasHat = false; Name = "Don" }
    let predicate = buildExpr<BoolRec,bool> "let fxy = (fun x y -> x = \"Don\" and not y) in fxy Name HasHat"
    let result = predicate testRecord
    Assert.True(result) 

[<Fact>]
let ``lambda parameters should close over external bindings`` () = 
    let testRecord = { HasHat = false; Name = "Don" }
    let predicate = buildExpr<BoolRec,bool> "let x = 1 in let fy = (fun y -> x = 1) in fy Name"
    let result = predicate testRecord
    Assert.True(result) 

[<Fact>]
let ``lambda parameters should subscope external bindings`` () = 
    let testRecord = { HasHat = false; Name = "Don" }
    let predicate = buildExpr<BoolRec,bool> "let x = 1 in let fx = (fun x -> x = \"Don\") in fx Name"
    let result = predicate testRecord
    Assert.True(result) 

[<Fact>]
let ``should support int addition`` () = 
    let predicate = buildExpr<unit,bool> "1 + 2 = 3"
    let result = predicate ()
    Assert.True(result) 

[<Fact>]
let ``should support int subtraction`` () = 
    let predicate = buildExpr<unit,bool> "2 - 2 = 0"
    let result = predicate ()
    Assert.True(result)
    
[<Fact>]
let ``should support int division`` () = 
    let predicate = buildExpr<unit,bool> "4 / 2 = 2"
    let result = predicate ()
    Assert.True(result) 

[<Fact>]
let ``should support int multiplication`` () = 
    let predicate = buildExpr<unit,bool> "2 * 2 = 4"
    let result = predicate ()
    Assert.True(result)  

[<Fact>]
let ``should support decimal addition`` () = 
    let predicate = buildExpr<unit,bool> "1.0 + 2.0 = 3.0"
    let result = predicate ()
    Assert.True(result) 

[<Fact>]
let ``should support decimal subtraction`` () = 
    let predicate = buildExpr<unit,bool> "2.0 - 2.0 = 0"
    let result = predicate ()
    Assert.True(result)
    
[<Fact>]
let ``should support decimal division`` () = 
    let predicate = buildExpr<unit,bool> "4 / 2.0 = 2.0"
    let result = predicate ()
    Assert.True(result) 

[<Fact>]
let ``should support decimal multiplication`` () = 
    let predicate = buildExpr<unit,bool> "2.0 * 2.0 = 4.0"
    let result = predicate ()
    Assert.True(result)  

[<Fact>]
let ``should support if-then-else`` () = 
    let testRec = { Score = 101.0 } 
    let predicate = buildExpr<TestFloatRec,bool> "(if Score >= 100 then 5 else 10) = 5"
    let result = predicate testRec
    Assert.True(result)  

[<Fact>]
let ``should support if-then-else without an explicit subexpression`` () = 
    let testRec = { Score = 101.0 } 
    let predicate = buildExpr<TestFloatRec,bool> "if Score >= 100 then true else false"
    let result = predicate testRec
    Assert.True(result)  

[<Fact>]
let ``should support if-then-else with initial/final spacing`` () = 
    let testRec = { Score = 101.0 } 
    let predicate = buildExpr<TestFloatRec,bool> "( if Score >= 100 then 5 else 10 ) = 5"
    let result = predicate testRec
    Assert.True(result)  

[<Fact>]
let ``should support iterative incremental tuple building`` () = 
    let predicate = buildExpr<unit,bool> "{1 .. 5} = (1, 2, 3, 4, 5)"
    let result = predicate ()
    Assert.True(result)  

[<Fact>]
let ``should support iterative incremental tuple building with jumps`` () = 
    let predicate = buildExpr<unit,bool> "{0 .. 2 .. 10} = (0, 2, 4, 6, 8, 10)"
    let result = predicate ()
    Assert.True(result)  

[<Fact>]
let ``should treat tuples properly without subexpressions`` () = 
    let predicate = buildExpr<unit,bool> "(1,2,3) = (1,2,3)"
    let result = predicate ()
    Assert.True(result)  

[<Fact>]
let ``should treat tuples properly even when in a left subexpression`` () = 
    let predicate = buildExpr<unit,bool> "((1,2,3)) = (1,2,3)"
    let result = predicate ()
    Assert.True(result)  

[<Fact>]
let ``should treat tuples properly even when in a right subexpression`` () = 
    let predicate = buildExpr<unit,bool> "(1,2,3) = ((1,2,3))"
    let result = predicate ()
    Assert.True(result)        

[<Fact>]
let ``should support recursion`` () = 
    let predicate = buildExpr<unit,bool> "let fx = (fun x -> (if x = 0 then true else fx (x - 1))) in fx 5"
    let result = predicate ()
    Assert.True(result)  

[<Fact>]
let ``tuples should be indexable`` () = 
    let predicate = buildExpr<unit,bool> "(1,2,3)[1] = 2"
    let result = predicate ()
    Assert.True(result)  


[<Fact>] 
let ``should support nested tuples internally`` () =
    let predicate = buildExpr<unit,bool> "((1,2), (3,4), (5,6)).[1] = (3,4)"
    Assert.True(predicate())

[<Fact>]
let ``expressions should not need spaces`` () = 
    let predicate = buildExpr<unit,int> "3+5"
    let result = predicate ()
    Assert.Equal(8, result)  

type InnerType = { Things: string array }
type OuterType = { Stuff: InnerType }

[<Fact>]
let ``tuple parsing should not cause problems with nested property calls`` () =
    let input = { Stuff = { Things = [|"one"; "two"; "three"|]} }
    let predicate = buildExpr<OuterType, bool> "(Stuff.Things, ('four','five')) = (('one','two','three'),('four','five'))"
    let result = predicate input
    Assert.True(result)

[<Fact>]
let ``union operator should union tuples`` () =
    let predstr = @"(1,2,3) \/ (4,5,6) = (1,2,3,4,5,6)"
    let predicate = buildExpr<unit, bool> predstr
    let result = predicate ()
    Assert.True(result)

[<Fact>]
let ``union operator should union tuples with singltons on the right`` () =
    let predstr = @"(1,2,3) \/ 5 = (1,2,3,5)"
    let predicate = buildExpr<unit, bool> predstr
    let result = predicate ()
    Assert.True(result)

[<Fact>]
let ``union operator should union tuples with singltons on the left`` () =
    let predstr = @"5 \/ (1,2,3) = (5,1,2,3)"
    let predicate = buildExpr<unit, bool> predstr
    let result = predicate ()
    Assert.True(result)

[<Fact>]
let ``null should act as identity for the union operator`` () =
    let predstr = @"null \/ (1,2,3) = (1,2,3)"
    let predicate = buildExpr<unit, bool> predstr
    let result = predicate ()
    Assert.True(result)
    let predstr = @"(1,2,3) \/ null = (1,2,3)"
    let predicate = buildExpr<unit, bool> predstr
    let result = predicate ()
    Assert.True(result)

[<Fact>]
let ``intersection operator should find the intersection of tuples`` () =
    let predicate = buildExpr<unit, bool> @"(2,3,4) /\ (3,4,5) = (3,4)" 
    let result = predicate ()
    Assert.True(result)

[<Fact>]
let ``intersection operator should find the intersection of tuples with a singleton`` () =
    let predicate = buildExpr<unit, bool> @"(2,3,4) /\ 3 = 3" 
    let result = predicate ()
    Assert.True(result)

[<Fact>]
let ``intersection operator on null should return null`` () =
    let predicate = buildExpr<unit, bool> @"(2,3,4) /\ null = null" 
    let result = predicate ()
    Assert.True(result)
    let predicate = buildExpr<unit, bool> @"null /\ (2,3,4) = null" 
    let result = predicate ()
    Assert.True(result)

[<Fact>]
let ``hasintersection operator should show if tuples have intersection`` () =
    let predicate = buildExpr<unit, bool> @"(2,3,4) /?\ (3,4,5)" 
    let result = predicate ()
    Assert.True(result)

[<Fact>]
let ``hasintersection operator should show if tuples don't have intersection`` () =
    let predicate = buildExpr<unit, bool> @"(1,2,3) /?\ (4,5,6)" 
    let result = predicate ()
    Assert.False(result)

[<Fact>]
let ``hasintersection operator should work with single elements on the right`` () =
    let predicate = buildExpr<unit, bool> @"(1,2,3) /?\ 2" 
    let result = predicate ()
    Assert.True(result)
    let predicate = buildExpr<unit, bool> @"(1,2,3) /?\ 4" 
    let result = predicate ()
    Assert.False(result)

[<Fact>]
let ``hasintersection operator should work with single elements on the left`` () =
    let predicate = buildExpr<unit, bool> @"2 /?\ (1,2,3)" 
    let result = predicate ()
    Assert.True(result)
    let predicate = buildExpr<unit, bool> @"4 /?\ (1,2,3)" 
    let result = predicate ()
    Assert.False(result)

[<Fact>]
let ``hasintersection operator should consider null to be the empty set`` () =
    let predicate = buildExpr<unit, bool> @"null /?\ (1,2,3)" 
    let result = predicate ()
    Assert.False(result)
    let predicate = buildExpr<unit, bool> @"(1,2,3) /?\ null" 
    let result = predicate ()
    Assert.False(result)
    let predicate = buildExpr<unit, bool> @"null /?\ null" 
    let result = predicate ()
    Assert.False(result)


[<Fact>] 
let ``Any indexing on null should return null`` () =
    let predicate = buildExpr<unit, bool> @"null.[5] = null" 
    let result = predicate ()
    Assert.True(result)    

[<Fact>] 
let ``Any out of bounds indexing should return null`` () =
    let predicate = buildExpr<unit, bool> @"(1,2,3)[6] = null" 
    let result = predicate ()
    Assert.True(result)    

//
// Wish List / Ideas
//


//[<Fact>] // Experimental
let ``should support safe while syntax`` () = 
    let predicate = buildExpr<unit,bool> "let x = 1 in { while x < 5 do x <- x + 1 } x = 4"
    let result = predicate ()
    Assert.True(result)  

