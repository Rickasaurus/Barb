module Barb.Helpers

open System
open System.Text
open System.Text.RegularExpressions
open System.Reflection
open System.ComponentModel
open System.Linq
open System.Collections.Concurrent
open System.Collections.Generic

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
