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

type StringTokenType =
    | Normal
    | Quoted

type ParseMode =
    | Standard
    | NumMode
    | QuoteMode of char
    | Escaped of ParseMode

let (|Single|_|) (mStr: string) (text: string) =
    if text.StartsWith(mStr) then 
        Some (text.Substring(mStr.Length))
    else None

let (|TokenToVal|_|) (mStr: string) (result: ExprTypes) (text: string) =
    if text.StartsWith(mStr) then 
        Some (result, text.Substring(mStr.Length))
    else None

let (|TokensToVal|_|) (mStrs: string list) (result: ExprTypes) (text: string) =
    match mStrs |> List.tryFind (fun mStr -> text.StartsWith(mStr)) with
    | Some (matched) -> Some (result, text.Substring(matched.Length))
    | None -> None

let allIndicesOf (iStr: string) (text: string) =
    [
        let lastIndex = ref 0
        while !lastIndex <> -1 do
            let index = text.IndexOf(iStr, !lastIndex)
            if index <> -1 then
                yield index
                lastIndex := index + 1
            else lastIndex := index
    ]

type CaptureTypes = 
    /// Begin, Delimiter, End
    | DelimitedCapture of string * string * string * ExprTypes
    /// Begin, End
    | Capture of string * string

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
    let rec (|Capture|_|) (bStr: string) (eStr: string) (text: string) =
        if text.StartsWith(bStr) then 
            let bStrEnd = bStr.Length
            let eStrStart = text.IndexOf(eStr, bStr.Length)
            if eStrStart = -1 then 
                failwith ("Unmatched Subexpression starting with: " + bStr) 
            let eStrEnd = eStr.Length + eStrStart
            let contents = text.Substring(bStrEnd, eStrStart - bStrEnd)
            let remainder = if eStrEnd < text.Length then text.Substring(eStrEnd) else ""
            Some (contents, remainder)
        else None
    and (|DelimitedCapture|_|) (bStr: string) (delimStr: string) (eStr: string) (text: string) =
        match text with
        | Capture bStr eStr (contents, remainder) -> 
            let contentEnd = contents.Length
            let _, substrings = 
                allIndicesOf delimStr text 
                |> (fun seps -> seps @ [contentEnd + 1])
                |> List.fold (fun (last, texts) next -> next, contents.Substring(last, next - last - 1) :: texts) (0, [])
            Some (substrings |> List.rev, remainder)
        | _ -> None  
    and parseProgramInner (str: string) (parsed: ExprTypes list) =
        let returned, remainder = 
            match str with
            | Single " " rem -> None, rem
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
            | TokensToVal ["!"; "not "] (BoolToBool not) res
            | TokensToVal ["&"; "&&"; "and "] (Infix (BoolToBoolToBool (&&))) res
            | TokensToVal ["|"; "||"; "or "] (Infix (BoolToBoolToBool (||))) res -> let v, rem = res in Some(v), rem            
            // String Literal Matches
            | Capture "\"" "\"" (contents, remainder)
            | Capture "'" "'"   (contents, remainder) -> Some <| Obj contents, remainder
            // Sub Expressions
            | DelimitedCapture "(" "," ")" (tupleContents, remainder) -> Some <| Tuple (tupleContents |> List.map (fun tc -> SubExpression <| parseProgramInner tc [])), remainder
            | Capture "(" ")" (contents, remainder) -> Some <| SubExpression (parseProgramInner contents []), remainder
            | Capture "[" "]" (contents, remainder) -> Some <| IndexArgs (SubExpression (parseProgramInner contents [])), remainder
            | FreeToken ["."; " "; "("] (token, remainder) ->
                match getMember token with
                | Some (expr) ->  Some <| expr, remainder
                | None -> Some <| Unknown token, remainder
            | _ -> failwith (sprintf "Unexpected Text: %A" str)
        let soFar = 
            match returned with
            | None -> parsed
            | Some (value) -> value :: parsed
        match remainder with
        | "" -> soFar
        | _ -> parseProgramInner remainder soFar
    parseProgramInner startText []  |> List.rev

let tokenizeString (str: string) = 
    let whitespace = [| yield ' '; yield! Environment.NewLine |]
    let isWhitespace ci = 
        ci < 0 || ci >= str.Length || whitespace.Contains(str.[ci])
    let tokenChars = [| ','; ')'; '('; '['; ']'; '.' |]
    let isTokenChar ci =
        not (isWhitespace ci) && tokenChars.Contains(str.[ci])
    let quoteChars = [| '"'; ''' |]
    let isQuoteChar ci = 
        not (isWhitespace ci) && quoteChars.Contains(str.[ci])
    let isLegalNeighbor ci = 
        isWhitespace ci || tokenChars.Contains(str.[ci])
    let numChars = [| '0'; '1'; '2'; '3'; '4'; '5'; '6'; '7'; '8'; '9' |] 
    let endOfString = str.Length
    let rec findTokens index tokenAcc parseMode pairs = 
        match index, tokenAcc, parseMode with
        | ci, "", _ when ci = endOfString -> pairs
        | ci, acc, parseMode when ci = endOfString -> (acc, parseMode) :: pairs
        // Parse out escaped chars
        | ci, acc, Escaped(lastMode) -> findTokens (ci + 1) (acc + (string str.[ci])) lastMode pairs
        // Set escape mode on \
        | ci, acc, mode when str.[ci] = '\\' -> findTokens (ci + 1) acc (Escaped mode) pairs
        // Parse out chars which always count as their own tokens (unless in quotes or escaped)
        | ci, acc, Standard when isTokenChar ci -> 
            let paramToken = (string str.[ci]), parseMode
            if acc = "" then findTokens (ci + 1) "" Standard (paramToken :: pairs)
            else findTokens (ci + 1) "" Standard (paramToken :: (acc, parseMode) :: pairs)   

        // Open Quote
        | ci, acc, Standard when isQuoteChar ci -> findTokens (ci + 1) acc (QuoteMode str.[ci]) pairs
        // Close Quote
        | ci, acc, QuoteMode(qc) when qc = str.[ci] -> findTokens (ci + 1) "" Standard ((acc, parseMode) :: pairs)

        // Start NumMode
        | ci, acc, Standard when numChars.Contains(str.[ci]) && isLegalNeighbor (ci - 1) -> findTokens (ci + 1) (string str.[ci]) NumMode pairs
        // Continue NumMode 
        | ci, acc, NumMode when numChars.Contains(str.[ci]) || str.[ci] = '.' -> findTokens (ci + 1) (acc + (string str.[ci])) parseMode pairs
        // End NumMode
        | ci, acc, NumMode when isLegalNeighbor ci -> findTokens (ci) "" Standard ((acc, parseMode) :: pairs)

        // Non-Quoted Token Start
        | ci, acc, Standard when not <| isWhitespace ci && acc = "" -> findTokens (ci + 1) (acc + (string str.[ci])) Standard pairs
        // Non-Quoted Token End
        | ci, acc, Standard when isLegalNeighbor ci && acc <> "" -> findTokens (ci + 1) "" Standard ((acc, parseMode) :: pairs)

        // Whitespace
        | ci, acc, Standard when isWhitespace ci -> findTokens (ci + 1) "" parseMode pairs
        // Continue
        | ci, acc, _ -> findTokens (ci + 1) (acc + (string str.[ci])) parseMode pairs
    findTokens 0 "" Standard [] 
    |> List.map (fun (token, parseMode) -> let tokenType = match parseMode with | Standard | NumMode -> Normal | QuoteMode (qc) -> Quoted | _ -> failwith (sprintf "Unexepcted Token Type: %A" parseMode)
                                           token, tokenType)
    |> List.rev

type TokenPair = string * StringTokenType
 
let inline (|ResolveConstants|_|) token =
    match token with
    | "null" -> Some <| Obj null
    | "true" -> Some <| Bool true
    | "false" -> Some <| Bool false
    | _ -> None  
  
let inline (|CompareOperation|_|) token =
    match token with
    | "=" | "==" -> Some <| Infix (ObjToObjToBool objectsEqual)
    | "<>" | "!=" -> Some <| Infix (ObjToObjToBool (fun o1 o2 -> not (objectsEqual o1 o2)))
    | ">" -> Some <| Infix (ObjToObjToBool (compareObjects (>)))
    | "<" -> Some <| Infix (ObjToObjToBool (compareObjects (<)))
    | ">=" -> Some <| Infix (ObjToObjToBool (compareObjects (>=)))
    | "<=" -> Some <| Infix (ObjToObjToBool (compareObjects (<=)))
    | _ -> None

let inline (|NotOperation|_|) token =
    match token with
    | "!" | "not" -> Some <| (BoolToBool not) 
    | _ -> None

let inline (|ScopedBooleanOperation|_|) token = 
    match token with
    | "and" -> Some <| Infix (BoolToBoolToBool (&&))
    | "or"  -> Some <| Infix (BoolToBoolToBool (||))
    | _ -> None

let inline (|BooleanOperation|_|) token = 
    match token with
    | "&" | "&&" -> Some <| Infix (BoolToBoolToBool (&&))
    | "|" | "||" -> Some <| Infix (BoolToBoolToBool (||))
    | _ -> None

let inline (|GetOperation|_|) getMember (token: string) = 
    getMember token

let inline (|BeginBinding|_|) getMember (token: string) =
    match token with
    | "let" -> Some() | _ -> None

let inline (|BeginIndexer|_|) token =
    match token with
    | "[" -> Some() | _ -> None

let inline (|EndIndexer|_|) token =
    match token with
    | "]" -> Some() | _ -> None

let inline (|BeginSubExpression|_|) token =
    match token with
    | "(" -> Some() | _ -> None 
  
let inline (|EndSubExpression|_|) token =
    match token with
    | ")" -> Some() | _ -> None

let inline (|TupleIndicator|_|) token =
    match token with
    | "," -> Some() | _ -> None

let inline (|CallIndicator|_|) token =
    match token with
    | "." -> Some Invoke | _ -> None

type SubExpressionType =
    | StandardExpression
    | TupleExpression
    | BindingExpression

let parseTokens getMember tokens = 
    let rec parseTokensInner tokens (collectedResult: ExprTypes list) (result: ExprTypes list) expressionType = 
        match tokens, collectedResult with
        | [], [] -> result, [], expressionType
        | [], ch :: ct -> parseTokensInner [] ct (ch :: result) expressionType
        | head :: rest, collected -> 
            let contents, tokenType = head
            match tokenType with
            | Quoted -> parseTokensInner rest (Obj contents :: collectedResult) result expressionType
            | Normal -> 
                match contents with
                | BeginSubExpression -> 
                    let exprs, unused, returnedExprType = parseTokensInner rest [] [] StandardExpression
                    match exprs, returnedExprType with
                    | [], StandardExpression -> parseTokensInner unused (Unit :: collectedResult) result expressionType
                    | contents, StandardExpression -> parseTokensInner unused (SubExpression exprs :: collectedResult) result expressionType
                    | contents, TupleExpression -> parseTokensInner unused (Tuple exprs :: collectedResult) result expressionType
                    | _ -> failwith (sprintf "Unexpected Expression Type in SubExpression: %A" returnedExprType)
                | EndSubExpression -> 
                    match expressionType with
                    | StandardExpression -> collectedResult |> List.rev, rest, expressionType
                    | TupleExpression -> 
                        let revCollected = collectedResult |> List.rev
                        (SubExpression revCollected :: result), rest, expressionType
                    | _ -> failwith (sprintf "Unexpected Expression Type in SubExpression: %A" expressionType)
                | TupleIndicator ->
                    let reversedResult = collectedResult |> List.rev 
                    parseTokensInner rest [] (SubExpression reversedResult :: result) TupleExpression
                | BeginIndexer -> 
                    let exprs, unused, returnedExprType = parseTokensInner rest [] [] StandardExpression
                    parseTokensInner unused (IndexArgs (SubExpression exprs) :: collectedResult) result expressionType
                | EndIndexer ->
                    collectedResult |> List.rev, rest, expressionType
                | ScopedBooleanOperation op -> 
                    match collectedResult |> List.rev with
                    | [] ->  parseTokensInner rest (op :: []) result expressionType
                    | supExpr -> parseTokensInner rest (op :: SubExpression supExpr :: []) result expressionType
                | BooleanOperation op
                | CallIndicator op
                | ResolveConstants op
                | NotOperation op
                | CompareOperation op 
                | GetOperation getMember op -> parseTokensInner rest (op :: collectedResult) result expressionType
                | unrecognized -> parseTokensInner rest (Unknown unrecognized :: collectedResult) result expressionType
                | _ -> failwith (sprintf "Unexpected Token: %A" head)
    parseTokensInner tokens [] [] StandardExpression
