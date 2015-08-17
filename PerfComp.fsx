#r @"src\Barb\bin\Release\Barb.dll"

open Barb.Parse

let text = StringWindow("-1234.5678", 0u)

let result = match text with | Num x -> x
let whileResult = match text with | WhileNum x -> x
let recResult = match text with | RecNum x -> x

let limit = 1L * 1000L * 1000L * 10L

let numbers = [0L .. limit]

#time

let speedTest =
    numbers
    |> List.map (fun x -> match text with | Num x -> x)

let recSpeedTest =
    numbers
    |> List.map (fun x -> match text with | RecNum x -> x)

let whileSpeedTest =
    numbers
    |> List.map (fun x -> match text with | WhileNum x -> x)


// **** Parsing 123.456 10m times
// Original (100%)
// Real: 00:00:17.306, CPU: 00:00:17.815, GC gen0: 548, gen1: 149, gen2: 2
// Recursive function p/q handling  (36.3%) (36.0%)
// Real: 00:00:11.021, CPU: 00:00:11.403, GC gen0: 345, gen1: 126, gen2: 2
// While loop p/q handling          (53.8%) (52.6%)
// Real: 00:00:07.988, CPU: 00:00:08.439, GC gen0: 281, gen1: 100, gen2: 2

// **** Parsing 1234.5678 10m times
// Original (100%)
// Real: 00:00:18.054, CPU: 00:00:18.642, GC gen0: 561, gen1: 151, gen2: 2
// Recursive function p/q handling  (31.0%) (29.5%)
// Real: 00:00:12.462, CPU: 00:00:13.150, GC gen0: 345, gen1: 124, gen2: 2
// While loop p/q handling          (54.5%) (52.6%)
// Real: 00:00:08.205, CPU: 00:00:08.829, GC gen0: 281, gen1: 100, gen2: 2

// **** Parsing 123456789.123456789 10m times
// Original (100%)
// Real: 00:00:18.402, CPU: 00:00:19.125, GC gen0: 753, gen1: 174, gen2: 3
// Recursive function p/q handling  (29.8%) (28.9%)
// Real: 00:00:12.917, CPU: 00:00:13.603, GC gen0: 346, gen1: 126, gen2: 2
// While loop p/q handling          (54.3%) (52.5%)
// Real: 00:00:08.404, CPU: 00:00:09.079, GC gen0: 282, gen1: 100, gen2: 2

// *** Parsing -1234.5678 10m times
// Original (100%)
// Real: 00:00:23.206, CPU: 00:00:24.024, GC gen0: 562, gen1: 150, gen2: 2
// Recursive function p/q handling  (48.9%) (46.7%)
// Real: 00:00:11.867, CPU: 00:00:12.792, GC gen0: 346, gen1: 126, gen2: 2
// While loop p/q handling          (66.2%) (65.4%)
// Real: 00:00:07.830, CPU: 00:00:08.314, GC gen0: 281, gen1: 100, gen2: 2
