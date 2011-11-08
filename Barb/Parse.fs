module Barb.Parse

open System
open System.Text
open System.Text.RegularExpressions
open System.Reflection
open System.ComponentModel
open System.Linq
open System.Collections.Concurrent
open System.Collections.Generic

open Barb.Interop
open Barb.Representation

let (|SkipToken|_|) (mStr: string) (text: string) =
    if text.StartsWith(mStr) then 
        Some (None, text.Substring(mStr.Length))
    else None

let (|TokenToVal|_|) (mStr: string) (result: ExprTypes) (text: string) =
    if text.StartsWith(mStr) then 
        Some (Some(result), text.Substring(mStr.Length))
    else None

let (|TokensToVal|_|) (mStrs: string list) (result: ExprTypes) (text: string) =
    match mStrs |> List.tryFind (fun mStr -> text.StartsWith(mStr)) with
    | Some (matched) -> Some (Some(result), text.Substring(matched.Length))
    | None -> None

type CaptureParams = 
    {
        Begin: char
        Delim: char option
        End: char
        Func: ExprTypes list -> ExprTypes
    }

let (|Num|_|) (text: string) =
    let numChars = [| '0'; '1'; '2'; '3'; '4'; '5'; '6'; '7'; '8'; '9' |]
    let sb = new StringBuilder()
    if numChars.Contains(text.[0]) then
        let rec inner = 
            function
            | i, _ when i >= text.Length -> i
            | i, true when text.[i] = '.' -> i
            | i, false when text.[i] = '.' -> sb.Append(text.[i]) |> ignore; inner (i+1, true)
            | i, dotSeen when numChars.Contains(text.[i]) -> sb.Append(text.[i]) |> ignore; inner (i+1, dotSeen)
            | i, _ -> i
        let resi = inner (0, false)
        let tokenStr = Some (Obj (sb.ToString() :> obj))
        let rest = text.Substring(resi)
        Some (tokenStr, rest)
    else None
        

let (|TextCapture|_|) (b: char) (text: string) = 
    if text.[0] = b then 
        let sb = new StringBuilder()
        let rec findSafeIndex i =
            if i >= text.Length then failwith "Quotes not matched"
            elif text.[i] = '\\' then findSafeIndex(i + 1)
            elif text.[i] = b && text.[i - 1] <> '\\' then 
                let tokenStr = Some (Obj (sb.ToString() :> obj))
                let rest = text.Substring(i + 1)
                Some (tokenStr, rest)
            else sb.Append(text.[i]) |> ignore; findSafeIndex (i + 1)
        findSafeIndex 1
    else None  

let (|FreeToken|_|) (endTokens: string list) (text: string) =
    let endIndices = 
        endTokens 
        |> List.map (fun e -> text.IndexOf e)
        |> List.filter (fun i -> i > 0)
    match endIndices with
    | [] -> Some (text, "")
    | list -> 
        let index = list |> List.min 
        let tokenText = text.Substring(0, index)
        let remainder = text.Substring(index)
        Some (tokenText, remainder) 

let parseProgram (getMember: string -> ExprTypes option) (startText: string) = 
    let captureTypes = 
        [
            { Begin = '('; Delim = None;     End = ')'; Func = (function | [] -> Unit | exprs -> SubExpression exprs) }
            { Begin = '('; Delim = Some ','; End = ')'; Func = (fun exprs -> Tuple exprs) }
            { Begin = '['; Delim = None;     End = ']'; Func = (fun exprs -> IndexArgs <| SubExpression exprs) }
        ]

    let (|CaptureDelim|_|) currentCaptures (text: string) =
        match [ for cc in currentCaptures do if cc.Delim.IsSome && text.[0] = cc.Delim.Value then yield cc ] with
        | [] -> None
        | caps -> Some (caps, text.Substring(1))

    let (|CaptureEnd|_|) currentCaptures (text: string) =
        match [ for cc in currentCaptures do if text.[0] = cc.End then yield cc ] with
        | [] -> None
        | caps -> Some (caps, text.Substring(1))

    let (|CaptureBegin|_|) (text: string) =
        match [ for ct in captureTypes do if text.[0] = ct.Begin then yield ct ] with
        | [] -> None
        | caps -> Some (caps, text.Substring(1))

    let rec parseProgramInner (str: string) (result: ExprTypes list) (currentCaptures: CaptureParams list) =
        match str with
        | "" -> result, "", currentCaptures
        | SkipToken " " res
        | TokenToVal "." Invoke res
        | TokenToVal "()" Unit res
        | TokenToVal "null" (Obj null) res
        | TokenToVal "true" (Bool true) res 
        | TokenToVal "false" (Bool false) res 
        | TokensToVal ["=="; "="] (Infix (ObjToObjToBool objectsEqual)) res 
        | TokensToVal ["<>"; "!="] (Infix (ObjToObjToBool objectsNotEqual)) res
        | TokenToVal ">=" (Infix (ObjToObjToBool (compareObjects (>=)))) res
        | TokenToVal "<=" (Infix (ObjToObjToBool (compareObjects (<=)))) res
        | TokenToVal ">" (Infix (ObjToObjToBool (compareObjects (>)))) res
        | TokenToVal "<" (Infix (ObjToObjToBool (compareObjects (<)))) res 
        | TokensToVal ["!"; "not"] (BoolToBool not) res
        | TokensToVal ["&"; "&&"; "and"] (Infix (BoolToBoolToBool (&&))) res
        | TokensToVal ["|"; "||"; "or"] (Infix (BoolToBoolToBool (||))) res 
        | TextCapture '"' res
        | TextCapture ''' res 
        | Num res ->
            let v, rem = res 
            match v with
            | Some value -> parseProgramInner rem (value :: result) currentCaptures
            | None -> parseProgramInner rem result currentCaptures
        | CaptureBegin (cParams, crem) -> 
            let subExprs, rem, captures = parseProgramInner crem [] cParams
            let exprConstraint = 
                match captures with
                | [] -> failwith "Unexpected end of subexpression"
                | h :: [] -> h
                | list -> list |> List.filter (fun cap -> cap.Delim.IsNone) |> (function | h :: [] -> h | _ -> failwith "Ambiguous end of subexpression")
            let value = subExprs |> List.rev |> exprConstraint.Func 
            parseProgramInner rem (value :: result) currentCaptures
        | CaptureDelim currentCaptures (cParams, crem) -> 
            let subExprs, rem, captures = parseProgramInner crem [] cParams
            [SubExpression (result |> List.rev)] @ subExprs, rem, captures 
        | CaptureEnd currentCaptures (cParams, crem) ->
            [SubExpression (result |> List.rev)], crem, cParams 
        | FreeToken ["."; " "; "("; ")"; ","; "."; "]"; "["] (token, rem) ->
            let memberToken = 
                match getMember token with
                | Some (expr) -> expr
                | None -> Unknown token
            parseProgramInner rem (memberToken :: result) currentCaptures
        | str -> parseProgramInner (str.Substring(1)) result currentCaptures
    let res, _, _ = parseProgramInner startText [] [] in res |> List.rev
