module PredicateLanguageOOOTests

open Barb.Compiler

open Xunit

type DudeRecord = { Name: string; Sex: char }

type BoolRec = { HasHat: bool; Name: string }

[<Fact>]
let ``predicate language should preserve left-to-right order of operations with parens`` () = 
    let testRecord = { HasHat = true; Name = "Howard" }
    let predicate = buildExpr<BoolRec,bool> "HasHat and (Name = \"Howard\")"
    let result = predicate testRecord
    Assert.True(result)    

[<Fact>]
let ``predicate language should preserve left-to-right order of operations with record bool`` () = 
    let testRecord = { HasHat = true; Name = "Howard" }
    let predicate = buildExpr<BoolRec,bool> "HasHat and Name = \"Howard\""
    let result = predicate testRecord
    Assert.True(result)    

[<Fact>]
let ``predicate language should preserve left-to-right order of operations with explicit bool`` () = 
    let testRecord = { HasHat = false; Name = "Don" }
    let predicate = buildExpr<BoolRec,bool> "HasHat = false and Name = \"Don\""
    let result = predicate testRecord
    Assert.True(result)    

[<Fact>]
let ``predicate language should evalute order of boolean ops correctly`` () = 
    let testRecord = { HasHat = true; Name = "Don" }
    let predicate = buildExpr<BoolRec,bool> "true and HasHat or false"
    let result = predicate testRecord
    Assert.True(result)    

[<Fact>]
let ``predicate language should use correct order of operations`` () = 
    let predicate = buildExpr<unit,bool> "1 + 4 / 2 + 5 = 8"
    let result = predicate ()
    Assert.True(result)  

type NumRec = 
    {
        Num: int
    }
    
[<Fact>]
let ``predicate language should use correct order of operations with getters 1`` () = 
    let predicate = buildExpr<NumRec,bool> "Num + 4 / 4 = 3"
    let result = predicate { Num = 2 }
    Assert.True(result)    

[<Fact>]
let ``predicate language should use correct order of operations with getters 2`` () = 
    let predicate = buildExpr<NumRec,bool> "2 + Num / 4 = 3"
    let result = predicate { Num = 4 }
    Assert.True(result)

[<Fact>]
let ``predicate language should use correct order of operations with getters 3`` () = 
    let predicate = buildExpr<NumRec,bool> "2 + 2 / Num = 3"
    let result = predicate { Num = 2 }
    Assert.True(result)