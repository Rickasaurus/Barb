module Barb.Tests.ReduceTests


open Barb.Parse
open Barb.Reduce
open Barb.Compiler
open Barb.Representation

open Xunit

[<Fact>]
let ``barb should properly handle recursion of summing 100000`` () = 
    let pred = """let recurse = fun i -> if i < 100000 then recurse (i + 1) else i in recurse 0"""
    let func = buildExpr<unit, int>(pred)
    let res = func()
    Assert.Equal(100000, res)

type IntHolder = { Num: int }

[<Fact>]
let ``barb should properly handle recursion of summing 100000 on final reduction`` () =
    let pred = """let recurse = fun i -> if i < 100000 then recurse (i + 1) else i in recurse Num"""
    let func = buildExpr<IntHolder, int>(pred)
    let res = func({ Num = 0 })
    Assert.Equal(100000, res)

