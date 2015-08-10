module internal Barb.Helpers

open System
open System.Text
open System.Text.RegularExpressions
open System.Reflection
open System.ComponentModel
open System.Linq
open System.Linq.Expressions
open System.Collections.Concurrent
open System.Collections.Generic
open Microsoft.FSharp.Quotations
open Microsoft.FSharp.Reflection

module Option =
    let tryResolve (func: unit -> _ option) (opt: _ option) =
        match opt with
        | Some value -> Some value
        | None -> func()


let memoizeBy inputToKey f =
    let cache = ConcurrentDictionary<_, _>()
    fun x ->
        let k = inputToKey x
        if cache.ContainsKey(k) then cache.[k]
        else 
            let res = f x
            cache.[k] <- res
            res

module List =
    let allMaxBy (func: 'a -> int) (list: 'a list) =
        list 
        |> List.fold (fun (wins,str) i ->
            let newstr = func i
            if newstr > str then [i], newstr
            elif newstr = str then i :: wins, str
            else wins, str) 
            ([], 0) 

    let (|AllMaxBy|_|) (func: 'a -> int) (list: 'a list) =
        let maxitems, maxlen = 
            list |> List.fold (fun (wins,str) i ->
                     let newstr = func i
                     if newstr > str then [i], newstr
                     elif newstr = str then i :: wins, str
                     else wins, str) 
                     ([], 0) 
        match maxitems with
        | [] -> None
        | items -> Some (items, maxlen)
       



/// Used to ease reflection when building typed arrays
/// Converts Array<object> to Array<T> where T is the given type.
/// Caching of these converters is used because the process to build them is quite slow.
type CachingReflectiveArrayBuilder () = 
    let builderMap = ref Map.empty<string, obj -> obj>
    static member ReturnTypedArrayBuilder<'a> () : obj -> obj = 
        (fun (args: obj) -> args :?> obj array |> Array.map (fun a -> a :?> 'a) :> obj)
    member this.BuildTypedArray (lType: System.Type) =
        let currentMap = !builderMap
        if Map.containsKey (lType.FullName) currentMap then
            currentMap.[lType.FullName]
        else
           let builder = typeof<CachingReflectiveArrayBuilder>
                            .GetMethod("ReturnTypedArrayBuilder", BindingFlags.Public ||| BindingFlags.NonPublic ||| BindingFlags.Static)
                            .MakeGenericMethod([|lType|])
                            .Invoke(null, null) 
                            :?> obj -> obj
           do builderMap := Map.add lType.FullName builder currentMap
           builder    

type CachingReflectiveIDictionaryLookup () = 
    let builderMap = ref Map.empty<string, (obj * obj) -> obj>
    static member ReturnTypedIDictionaryLookup<'k,'v> () : (obj * obj) -> obj =
        (fun (dict, key) -> let idict = dict :?> IDictionary<'k,'v>
                            let key = key :?> 'k
                            if idict.ContainsKey(key) then idict.[key] |> box else null)
    member this.GetTypedIDictionaryIndexer (kType: Type, vType: Type) =
        let currentMap = !builderMap
        let cacheKey = kType.FullName + "_" + vType.FullName
        if Map.containsKey cacheKey currentMap then
            currentMap.[cacheKey]
        else
           let builder = typeof<CachingReflectiveIDictionaryLookup>
                            .GetMethod("ReturnTypedIDictionaryLookup", BindingFlags.Public ||| BindingFlags.NonPublic ||| BindingFlags.Static)
                            .MakeGenericMethod([|kType; vType|])
                            .Invoke(null, null) 
                            :?> (obj * obj) -> obj
           do builderMap := Map.add cacheKey builder currentMap
           builder    

module FSharpExpr =            
    let getMethod = 
        function
        | Patterns.Call (_, m, _) when m.IsGenericMethod -> m.GetGenericMethodDefinition()
        | Patterns.Call (_, m, _) -> m
        | _ -> failwith "Incorrect getMethod Pattern"
    
    let X<'T> : 'T = Unchecked.defaultof<'T>

    let inline application prms expr = Expr.Application(expr, prms)
    let inline coerse typ expr = Expr.Coerce(expr, typ)

module FSharpType =
    /// Caches the constructors used to generate option types in order to speed reflection
    let private optionCtorCachedBuilder =   
        let inputToKey (containedType: System.Type, outerType: System.Type, caseName) = String.Format("{0}~{1}~{2}", containedType.FullName, outerType.FullName, caseName)
        let preComputeCtor (containedType, outerType: System.Type, caseName) = 
            outerType.MakeGenericType([|containedType|])
                |> Reflection.FSharpType.GetUnionCases
                |> Array.find (fun c -> c.Name = caseName)
                |> Reflection.FSharpValue.PreComputeUnionConstructor
        memoizeBy inputToKey preComputeCtor

    /// Create an option type Some instance
    let MakeOptionSome (stype: System.Type) (value: _) =
        let ctor = optionCtorCachedBuilder(stype, typedefof<option<_>>, "Some")
        ctor [| value |]

    /// Create an option type None instance (in most cases it's better to use null instead)
    let MakeOptionNone (stype: System.Type) =
        let ctor = optionCtorCachedBuilder(stype, typedefof<option<_>>,"None")
        ctor [| |]
