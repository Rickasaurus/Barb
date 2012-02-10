namespace Barb.Lib

open System.Collections

module Lib = 
    let union (t1: IEnumerable) (t2: IEnumerable) =  Seq.append (t1 |> Seq.cast<obj>) (t2 |> Seq.cast<obj>) |> Seq.toArray
    let intersection (t1: IEnumerable) (t2: IEnumerable) = 
        seq {
            for i1 in t1 do
                for i2 in t2 do
                    if i1 = i2 then yield i1
        } |> Seq.toArray
    let hasIntersection (t1: IEnumerable) (t2: IEnumerable) =
        let t1obj = t1 |> Seq.cast<obj>
        let t2obj = t2 |> Seq.cast<obj>
        t1obj |> Seq.exists (fun i1 -> t2obj |> Seq.exists (fun i2 -> i1 = i2))

