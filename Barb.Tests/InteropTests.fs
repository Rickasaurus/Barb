module PredicateLanguageInteropTests

open System

open Barb.Compiler

open Xunit

type DudeRecord = { Name: string; Sex: char }

type ParentWithObject = { State: string; Data: obj }

type DudeRecordWithInt = { Name: string; Age: int }
    with member t.GetAge() = t.Age

[<Fact>]
let ``should support dynamic property lookup on unknown types`` () =
    let childRec = { Name = "Dude Duderson"; Age = 20 }
    let parentRec = { State = "Washington"; Data = childRec :> obj }
    let predicate = buildExpr<ParentWithObject,bool> "Data.Name = \"Dude Duderson\" and Data.Age < 30"
    let result = predicate parentRec
    Assert.True(result)

[<Fact>]
let ``should support dynamic method lookup on unknown types`` () =
    let childRec = { Name = "Dude Duderson"; Age = 20 }
    let parentRec = { State = "Washington"; Data = childRec :> obj }
    let predicate = buildExpr<ParentWithObject,bool> "Data.Name = \"Dude Duderson\" and Data.GetAge() < 30"
    let result = predicate parentRec
    Assert.True(result)

[<Fact>] 
let ``should support no argument methods`` () =
    let testRecord = { Name = " Dude Duderson "; Age = 20 }
    let predicate = buildExpr<DudeRecordWithInt,bool> "Name.Trim() = \"Dude Duderson\""
    let result = predicate testRecord
    Assert.True(result)

[<Fact>] 
let ``should support no argument methods on passed in constructs`` () =
    let testRecord = { Name = " Dude Duderson "; Age = 20 }
    let predicate = buildExpr<DudeRecordWithInt,bool> "GetAge() = 20"
    let result = predicate testRecord
    Assert.True(result)

[<Fact>] 
let ``should support single argument methods`` () =
    let testRecord = { Name = "Dude Duderson"; Age = 20 }
    let predicate = buildExpr<DudeRecordWithInt,bool> "Name.Contains(\"Dude Duderson\")"
    let result = predicate testRecord
    Assert.True(result)

[<Fact>] 
let ``should support multi-argument methods`` () =
    let testRecord = { Name = "Dude Duderson"; Age = 20 }
    let predicate = buildExpr<DudeRecordWithInt,bool> "Name.Substring(0, 4) = \"Dude\""
    let result = predicate testRecord
    Assert.True(result)

[<Fact>]
let ``should support invoking a method on the results of a method call`` () =
    let testRecord = { Name = "Dude Duderson"; Age = 20 }
    let predicate = buildExpr<DudeRecordWithInt,bool> "Name.Substring(0, 4).Length = 4"
    let result = predicate testRecord
    Assert.True(result)

[<Fact>]
let ``should support invoking a method on the results of a subexpression`` () =
    let testRecord = { Name = "Dude Duderson"; Age = 20 }
    let predicate = buildExpr<DudeRecordWithInt,bool> "(Name.Substring(0, 4)).Length = 4"
    let result = predicate testRecord
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
let ``should support property indexers`` () = 
    let testRecord = { Name = "Dude Duderson"; Table = new PropIndexerTester<int,int>([0..5] |> List.map (fun i -> i, i) |> Map.ofList) }
    let predicate = buildExpr<IndexerRecord<int,int>,bool> "Table.Item[0] = 0"
    let result = predicate testRecord
    Assert.True(result)    

[<Fact>]
let ``should support property indexers with strings`` () = 
    let testRecord = { Name = "Dude Duderson"; Table = new PropIndexerTester<string,string>(["one"; "two"; "three"] |> List.map (fun i -> i, i) |> Map.ofList) }
    let predicate = buildExpr<IndexerRecord<string,string>,bool> "Table.Item[\"two\"] = \"two\""
    let result = predicate testRecord
    Assert.True(result)    

[<Fact>]
let ``should directly support property indexers with strings`` () = 
    let testRecord = new PropIndexerTester<string,string>(["one"; "two"; "three"] |> List.map (fun i -> i, i) |> Map.ofList)
    let predicate = buildExpr<PropIndexerTester<string,string>,bool> "Item[\"two\"] = \"two\""
    let result = predicate testRecord
    Assert.True(result)    

type IndexedName = 
    {
        Name: string
        Index: int
    }   

[<Fact>]
let ``should correctly reduce the contents of indexers before applying`` () = 
    let testRecord = { Name = "Dude Duderson"; Index = 1 }
    let predicate = buildExpr<IndexedName,bool> "Name[Index] = \"u\""
    let result = predicate testRecord
    Assert.True(result)    

[<Fact>]
let ``should support using the results of calls to build incremental tuples`` () = 
    let testRecord = { Name = "Dude Duderson"; Index = 1 }
    let predicate = buildExpr<IndexedName,bool> "{Index .. 3} = (1, 2, 3)"
    let result = predicate testRecord
    Assert.True(result)  

type NameScores = 
    {
        Names1: string []
        Names2: string []
        Scores: float []
    }   

[<Fact>]
let ``should be able to index into a given array`` () = 
    let record = { Names1 = [|"John"; "Frank"|]; Names2 = [|""; "Franklin"|]; Scores = [|0.0; 0.8|] }     
    let predText = "Scores[1] = 0.8"
    let pred = buildExpr<NameScores,bool> predText
    let result = pred record
    Assert.True(result)

[<Fact>]
let ``should support recursive array iteration`` () =  
    let record = { Names1 = [||]; Names2 = [||]; Scores = [|0.0; 0.8; 0.5; 0.6|] }   
    let predText = 
        "let maxVal = (fun i best -> let nextbest = (if best > Scores[i] then best else Scores[i]) in
                                       (if i = 0 then nextbest else maxVal (i - 1) nextbest))
         in maxVal (Scores.Length - 1) 0 = 0.8"
    let pred = buildExpr<NameScores,bool> predText
    let result = pred record
    Assert.True(result)

[<Fact>]
let ``should support recursive array iteration _open_`` () =  
    let record = { Names1 = [||]; Names2 = [||]; Scores = [|0.0; 0.8; 0.5; 0.6|] }   
    let predText = 
        "let maxVal = fun i best -> let nextbest = if best > Scores[i] then best else Scores[i] in
                                       if i = 0 then nextbest else maxVal (i - 1) nextbest
         in maxVal (Scores.Length - 1) 0 = 0.8"
    let pred = buildExpr<NameScores,bool> predText
    let result = pred record
    Assert.True(result)

type TypeWithStaticMethod =
    { Meaningless: string }
    with static member IsTrue () = true

[<Fact>]
let ``should support invoking static methods`` () =
    let predicate = buildExpr<TypeWithStaticMethod,bool> "IsTrue()"
    let result = predicate { Meaningless = "yep" }
    Assert.True(result)         

[<Fact>]
let ``should support static methods on type names`` () = 
    let predicate = buildExpr<unit,bool> "String.IsNullOrEmpty(null)"
    let result = predicate ()    
    Assert.True(result)    

[<Fact>]
let ``should convert output to the correct type`` () = 
    let predicate = buildExpr<unit,string> "1"
    let result = predicate ()    
    Assert.Equal (result, "1")

type ValueCarrier = { Y: int }

[<Fact>]
let ``recursive calls should property preserve a lambda closure with external data`` () =
    let v = { Y = 1 }
    let func = buildExpr<ValueCarrier,int> "let fn = (fun x -> x + Y) in let Y = 2 in fn 1"
    let result = func v
    Assert.Equal(2, result)

[<Fact>]
let ``plus should concatinate strings`` () = 
    let func = buildExpr<unit,bool> "'hello' + ' ' + 'world' = 'hello world'"
    Assert.True(func())

[<Fact>]
let ``tuple implementation should support Length property`` () =
    let func = buildExpr<unit,bool> "let tpl = (1,2,3) in tpl.Length = 3"
    Assert.True(func())


//
// Wish List / Ideas
//

//[<Fact>] // Need support, but the reflection is difficult
let ``should support exporting nested tuples`` () =
    let predicate = buildExpr<unit,int array array> "((1,2), (3,4), (5,6))"
    Assert.Equal([|[|1; 2|]; [|3;4|]; [|5;6|]|], predicate())