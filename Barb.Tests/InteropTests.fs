module PredicateLanguageInteropTests

open System

open Barb.Compiler

open Xunit

type DudeRecord = { Name: string; Sex: char }

type ParentWithObject = { State: string; Data: obj }

type DudeRecordWithInt = { Name: string; Age: int }
    with member t.GetAge() = t.Age

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
let ``predicate language should support no argument methods`` () =
    let testRecord = { Name = " Dude Duderson "; Age = 20 }
    let dudePredicate = buildExpr<DudeRecordWithInt,bool> "Name.Trim() = \"Dude Duderson\""
    let result = dudePredicate testRecord
    Assert.True(result)

[<Fact>] 
let ``predicate language should support no argument methods on passed in constructs`` () =
    let testRecord = { Name = " Dude Duderson "; Age = 20 }
    let dudePredicate = buildExpr<DudeRecordWithInt,bool> "GetAge() = 20"
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
    let dudePredicate = buildExpr<DudeRecordWithInt,bool> "Name.Substring(0, 4) = \"Dude\""
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

[<Fact>]
let ``predicate language should directly support property indexers with strings`` () = 
    let testRecord = new PropIndexerTester<string,string>(["one"; "two"; "three"] |> List.map (fun i -> i, i) |> Map.ofList)
    let dudePredicate = buildExpr<PropIndexerTester<string,string>,bool> "Item[\"two\"] = \"two\""
    let result = dudePredicate testRecord
    Assert.True(result)    

type IndexedName = 
    {
        Name: string
        Index: int
    }   

[<Fact>]
let ``predicate language should correctly reduce the contents of indexers before applying`` () = 
    let testRecord = { Name = "Dude Duderson"; Index = 1 }
    let dudePredicate = buildExpr<IndexedName,bool> "Name[Index] = \"u\""
    let result = dudePredicate testRecord
    Assert.True(result)    

[<Fact>]
let ``predicate language should support using the results of calls to build incremental tuples`` () = 
    let testRecord = { Name = "Dude Duderson"; Index = 1 }
    let dudePredicate = buildExpr<IndexedName,bool> "{Index .. 3} = (1, 2, 3)"
    let result = dudePredicate testRecord
    Assert.True(result)  

type NameScores = 
    {
        Names1: string []
        Names2: string []
        Scores: float []
    }   

[<Fact>]
let ``predicate language should be able to index into a given array`` () = 
    let record = { Names1 = [|"John"; "Frank"|]; Names2 = [|""; "Franklin"|]; Scores = [|0.0; 0.8|] }     
    let predText = "Scores[1] = 0.8"
    let pred = buildExpr<NameScores,bool> predText
    let result = pred record
    Assert.True(result)

[<Fact>]
let ``predicate language should support recursive array iteration`` () =  
    let record = { Names1 = [||]; Names2 = [||]; Scores = [|0.0; 0.8; 0.5; 0.6|] }   
    let predText = 
        "let maxVal = (fun i best -> let nextbest = (if best > Scores[i] then best else Scores[i]) in
                                       (if i = 0 then nextbest else maxVal (i - 1) nextbest))
                      in maxVal (Scores.Length - 1) 0 = 0.8"
    let pred = buildExpr<NameScores,bool> predText
    let result = pred record
    Assert.True(result)

type TypeWithStaticMethod =
    { Meaningless: string }
    with static member IsTrue () = true

[<Fact>]
let ``predicate language should support invoking static methods`` () =
    let dudePredicate = buildExpr<TypeWithStaticMethod,bool> "IsTrue()"
    let result = dudePredicate { Meaningless = "yep" }
    Assert.True(result)         

[<Fact>]
let ``predicate language should support static methods on type names`` () = 
    let dudePredicate = buildExpr<unit,bool> "String.IsNullOrEmpty(null)"
    let result = dudePredicate ()    
    Assert.True(result)    

[<Fact>]
let ``predicate language should convert output to the correct type`` () = 
    let dudePredicate = buildExpr<unit,int> "1"
    let result = dudePredicate ()    
    Assert.Equal (result, 1)
