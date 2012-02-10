namespace Barb.Lib

module Lib = 
    let union (t1: obj seq) (t2: obj seq) = Seq.append t1 t2 |> Seq.toArray
    let intersection (t1: obj seq) (t2: obj seq) = 
        seq {
            for i1 in t1 do
                for i2 in t2 do
                    if i1 = i2 then yield i1
        } |> Seq.toArray
    let hasIntersection (t1: obj seq) (t2: obj seq) =
        t1 |> Seq.exists (fun i1 -> t2 |> Seq.exists (fun i2 -> i1 = i2))

