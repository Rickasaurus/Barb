module Barb.Tests.PredicateLanguageInteropTests

open System

open Barb.Compiler
open Barb.Representation

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
        PropTable: PropIndexerTester<'a,'b>
    }

[<Fact>]
let ``should support property indexers`` () = 
    let testRecord = { Name = "Dude Duderson"; PropTable = new PropIndexerTester<int,int>([0..5] |> List.map (fun i -> i, i) |> Map.ofList) }
    let predicate = buildExpr<IndexerRecord<int,int>,bool> "PropTable.Item[0] = 0"
    let result = predicate testRecord
    Assert.True(result)    
    let predicate = buildExpr<IndexerRecord<int,int>,bool> "PropTable.Item[2] = 2"
    let result = predicate testRecord
    Assert.True(result)    

[<Fact>]
let ``should support property indexers with strings`` () = 
    let testRecord = { Name = "Dude Duderson"; PropTable = new PropIndexerTester<string,string>(["one"; "two"; "three"] |> List.map (fun i -> i, i) |> Map.ofList) }
    let predicate = buildExpr<IndexerRecord<string,string>,bool> "PropTable.Item[\"two\"] = \"two\""
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

open System.Collections.Generic

type DictTestRecord =  
    {
        Dict: IDictionary<string,string>
    }

[<Fact>]
let ``should propertly index into IDictionary`` () =
    let testRecord = { Dict = [("one", "1"); ("two", "2"); ("three", "3")] |> dict }
    let predicate = buildExpr<DictTestRecord,bool> "Dict['two'] = '2'"
    let result = predicate testRecord
    Assert.True(result)

[<Fact>]
let ``should propertly call IDictionary ContainsKey`` () =
    let testRecord = { Dict = [("one", "1"); ("two", "2"); ("three", "3")] |> dict }
    let predicate = buildExpr<DictTestRecord,bool> "Dict.ContainsKey('two')"
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
    Assert.Equal<string> (result, "1")

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
let ``plus should work well with lossy on the right`` () =
    let func = buildExpr<unit,bool> "5.1 + 5 = 10.1"
    Assert.True(func())

[<Fact>]
let ``plus should work well with lossy on the left`` () =
    let func = buildExpr<unit,bool> "5 + 5.1 = 10.1"
    Assert.True(func())

type Nums = { x: obj; y: obj }

[<Fact>]
let ``plus should work with different types but may be slow`` () =
    let func = buildExpr<Nums,bool> "x + y = y + x"
    Assert.True(func({ x = 1; y = 2 }))
    Assert.True(func({ x = 1.0; y = 2.0 }))
    Assert.True(func({ x = 1u; y = 2u }))
    Assert.True(func({ x = "helleh"; y = "helleh" }))

[<Fact>]
let ``tuple implementation should support Length property`` () =
    let func = buildExpr<unit,bool> "let tpl = (1,2,3) in tpl.Length = 3"
    Assert.True(func())

type StrArrayType = { Strs: string array }

[<Fact>]
let ``records contents should be resolved within lambdas`` () =
    let func = buildExpr<StrArrayType, int> "let f = (fun i -> if i >= Strs.Length then 0 else (f (i + 1)) + Strs.[i].Length) in f 0"
    let res = func { Strs = [|"1"; "22"; "333" |] }
    Assert.Equal(6, res)

type StaticTestThingy () =
    static let mutable x = 0
    static member IncrementAndReturn 
        with get () = x <- x + 1; x

[<Fact>]
let ``when binding globals before execution should cause them to only be bound once`` () =
    let settings = { BarbSettings.Default with BindGlobalsWhenReducing = true; Namespaces = BarbSettings.Default.Namespaces |> Set.add "Barb.Tests" }
    let func = buildExprWithSettings<unit, int> settings "StaticTestThingy.IncrementAndReturn"
    Assert.Equal(func(), 1)
    Assert.Equal(func(), 1)
    Assert.Equal(func(), 1)

[<Fact>]
let ``should be able to create new objects with new`` () =
    let func = buildExpr<unit,bool> "new Uri(\"http://twitter.com\").IsWellFormedOriginalString()"
    Assert.True(func())

[<Fact>]
let ``should be able to create new objects without new`` () =
    let func = buildExpr<unit,bool> "Uri(\"http://twitter.com\").IsWellFormedOriginalString()"
    Assert.True(func())

//
// Wish List / Ideas
//

//[<Fact>] // Need support, but the reflection is difficult
let ``should support exporting nested tuples`` () =
    let predicate = buildExpr<unit,int array array> "((1,2), (3,4), (5,6))"
    Assert.Equal<int array array>([|[|1; 2|]; [|3;4|]; [|5;6|]|], predicate())

[<Fact>] 
let ``invoke on null should return null`` () =
    let predicate = buildExpr<unit, bool> "(null).Hello == null"
    Assert.True(predicate())

type RecordWithStatic = { CName: string }
    with static member Prefix = "Mr"

[<Fact>] 
let ``reflection should work in the presence of static properties`` ()  =
    let testRecord = { CName = "Dude Duderson"}
    let predicate = buildExpr<RecordWithStatic,bool> "Prefix + ' ' + CName = 'Mr Dude Duderson'"
    let result = predicate testRecord
    Assert.True(result)

type BoxBox = { Value : obj }

[<Fact>]
let ``should properly be able to reflect things out of an obj type`` () =
   let bb = { Value = "One Two Three" }
   let pred = buildExpr<BoxBox,obj> "Value"
   let res = pred bb
   Assert.Equal(bb.Value, res)

[<Fact>]
let ``should properly be able to call a unit method on an array with dotdot syntax`` () =
   let bb = { Value = [|" The Dude "; "The Word "|] }
   let pred = buildExpr<BoxBox,string []> "Value..Trim()"
   let res = pred bb 
   Assert.True((res = [|"The Dude"; "The Word"|]))

[<Fact>]
let ``should properly be able to call a method of one paramter on an array with dotdot syntax`` () =
   let bb = { Value = [|"The Dude"; "The Word"|] }
   let pred = buildExpr<BoxBox,string []> "Value..Remove(5)"
   let res = pred bb 
   Assert.True((res = [|"The D"; "The W"|]))

[<Fact>]
let ``should properly be able to call a method of two paramters on an array with dotdot syntax`` () =
   let bb = { Value = [|"The Dude"; "The Word"|] }
   let pred = buildExpr<BoxBox,string []> "Value..Replace('The', 'Da')"
   let res = pred bb 
   Assert.True((res = [|"Da Dude"; "Da Word"|]))
    