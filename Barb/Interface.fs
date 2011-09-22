namespace Barb

open Barb.Internals

module Compiler =
    let buildExpr<'I, 'O> predicate =
        let expression = buildExpression typeof<'I> predicate
        fun (input : 'I) -> 
            match expression input with
            | :? 'O as result -> result
            | untypedRes -> convertToTargetType typeof<'O> untypedRes 
                            |> function 
                               | Some (typedRes) -> typedRes :?> 'O               
                               | None -> failwith (sprintf "Unexpected output type/format: %A/%s" untypedRes (untypedRes.GetType().Name))

type BarbFunc<'I,'O> (predicate) =
    let func = Compiler.buildExpr<'I,'O> (predicate)
    member t.Execute (record: 'I) =     
        func record

