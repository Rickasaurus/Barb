module Barb.Parse

// TODO: 
// Change all this string manipulation to indexed lookups
// Do ascii lookup for Num, instead of a dumb list
// Add a way for captureTypes to know if a Repeat node has been already seen
 
open System
open System.Text
open System.Text.RegularExpressions
open System.Reflection
open System.ComponentModel
open System.Linq
open System.Collections.Concurrent
open System.Collections.Generic

open Barb.Helpers
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

type DelimType =
    | Single of string
    | Repeating of string

type CaptureParams = 
    {
        Begin: string
        Delims: DelimType list
        End: string
        Func: ExprTypes list -> ExprTypes
    }

open System.Numerics

let (|Num|_|) (text: string) =
    let numChars = [| '0'; '1'; '2'; '3'; '4'; '5'; '6'; '7'; '8'; '9' |]
    let sb = new StringBuilder()
    if numChars.Contains(text.[0]) then
        let rec inner = 
            function
            | i, dot when i >= text.Length -> i, dot
            | i, true when text.[i] = '.' -> i, true
            | i, false when text.[i] = '.' -> sb.Append(text.[i]) |> ignore; inner (i+1, true)
            | i, dotSeen when numChars.Contains(text.[i]) -> sb.Append(text.[i]) |> ignore; inner (i+1, dotSeen)
            | i, dot -> i, dot
        let resi, dot = inner (0, false)
        let tokenStr = 
            let resultStr = sb.ToString()
            if dot then match Decimal.TryParse(resultStr) with | true, num -> Obj num | _ -> Obj resultStr
            else match Int64.TryParse(resultStr) with | true, num -> Obj num | _ -> Obj resultStr
        let rest = text.Substring(resi)
        Some (Some (tokenStr), rest)
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

let bindFunction = 
    function 
    | h :: SubExpression([Unknown(name)]) :: [] -> Binding (name, h) 
    | list -> failwith (sprintf "Incorrect let binding syntax: %A" list)

let generateLambda = 
    function 
    | h :: SubExpression(names) :: [] ->
        let prms = names |> List.map (function | Unknown n -> n | other -> failwith (sprintf "Unexpected construct in lambda argument list: %A" other))
        Lambda (prms, List.empty, h) 
    | list -> failwith (sprintf "Incorrect lambda binding syntax: %A" list)

let generateIfThenElse =
    function
    | SubExpression(elseexpr) :: SubExpression(thenexpr) :: SubExpression(ifexpr) :: [] -> IfThenElse (ifexpr, thenexpr, elseexpr)
    | list -> failwith (sprintf "Incorrect if-then-else syntax: %A" list)

let captureTypes = 
    [
        { Begin = "(";    Delims = [];                             End = ")";  Func = (function | [] -> Unit | exprs -> SubExpression exprs) }
        { Begin = "(";    Delims = [Repeating ","];                End = ")";  Func = (fun exprs -> Tuple exprs) }
        { Begin = "[";    Delims = [];                             End = "]";  Func = (fun exprs -> IndexArgs <| SubExpression exprs) }
        { Begin = "let";  Delims = [Single "="];                   End = "in"; Func = bindFunction }
        { Begin = "var";  Delims = [Single "="];                   End = "in"; Func = bindFunction }
        { Begin = "(fun"; Delims = [Single "->"];                  End = ")";  Func = generateLambda }
        { Begin = "(if";  Delims = [Single "then"; Single "else"]; End = ")";  Func = generateIfThenElse }
    ]

let (|CaptureDelim|_|) currentCaptures (text: string) =
    let matches, delimLen = 
        [ 
            for cc in currentCaptures do
                match cc.Delims with
                | (Single delim) :: dt when text.StartsWith delim -> yield { cc with Delims = dt }, delim
                | (Repeating delim) :: dt when text.StartsWith delim -> yield cc, delim
                | (Repeating _) :: Repeating delim :: dt when text.StartsWith delim -> yield { cc with Delims = Repeating delim :: dt }, delim
                | (Repeating _) :: Single delim :: dt when text.StartsWith delim -> yield { cc with Delims = dt }, delim  
                | _ -> ()          
        ] 
        |> List.allMaxBy (fun (cc, matchedDelim) -> matchedDelim.Length)
        |> (fun (matches, delimlen) -> matches |> List.map (fun (cc, txt) -> cc), delimlen) 
    match matches with
    | [] -> None
    | onecap :: [] -> Some ([onecap], text.Substring(delimLen))
    | caps -> Some (caps, text.Substring(delimLen))

let (|CaptureEnd|_|) currentCaptures (text: string) =
    let matches, str = [ for cc in currentCaptures do if text.StartsWith cc.End then yield cc ] |> List.allMaxBy (fun ct -> ct.End.Length) 
    match matches with
    | [] -> None
    | onecap :: [] -> Some ([onecap], text.Substring(onecap.End.Length))
    | caps -> Some (caps, text.Substring(str))

let (|CaptureBegin|_|) (text: string) =
    let matches, str = [ for ct in captureTypes do if text.StartsWith ct.Begin then yield ct ] |> List.allMaxBy (fun ct -> ct.Begin.Length)
    match matches with
    | [] -> None
    | onecap :: [] -> Some ([onecap], text.Substring(onecap.Begin.Length))
    | caps -> Some (caps, text.Substring(str))

let parseProgram (startText: string) = 
    let rec parseProgramInner (str: string) (result: ExprTypes list) (currentCaptures: CaptureParams list) =
        match str with
        | "" -> result, "", currentCaptures
        | CaptureBegin (cParams, crem) -> 
            let subExprs, rem, captures = parseProgramInner crem [] cParams
            let exprConstraint = 
                match captures with
                | [] -> failwith "Unexpected end of subexpression"
                | h :: [] -> h
                | list -> list |> List.filter (fun cap -> List.isEmpty cap.Delims) |> (function | h :: [] -> h | _ -> failwith "Ambiguous end of subexpression")
            let value = subExprs |> List.rev |> exprConstraint.Func 
            parseProgramInner rem (value :: result) currentCaptures
        | CaptureDelim currentCaptures (cParams, crem) -> 
            let subExprs, rem, captures = parseProgramInner crem [] cParams
            [SubExpression (result |> List.rev)] @ subExprs, rem, captures 
        | CaptureEnd currentCaptures (cParams, crem) ->  
            match result with
            | [] -> result, crem, cParams
            | results -> [SubExpression (results |> List.rev)], crem, cParams 
        | SkipToken " " res
        | TokenToVal "." Invoke res
        | TokenToVal "()" Unit res
        | TokenToVal "null" (Obj null) res
        | TokenToVal "true" (Obj true) res 
        | TokenToVal "false" (Obj false) res 
        | TokensToVal ["=="; "="] (Infix (3, objectsEqual)) res 
        | TokensToVal ["<>"; "!="] (Infix (3, objectsNotEqual)) res
        | TokenToVal ">=" (Infix (3, compareObjects (>=))) res
        | TokenToVal "<=" (Infix (3, compareObjects (<=))) res
        | TokenToVal ">" (Infix (3, compareObjects (>))) res
        | TokenToVal "<" (Infix (3, compareObjects (<))) res 
        | TokensToVal ["!"; "not"] (Prefix notOp) res
        | TokensToVal ["&"; "&&"; "and"] (Infix (4, andOp)) res
        | TokensToVal ["|"; "||"; "or"] (Infix (4, orOp)) res 
        | TokenToVal "/" (Infix (1, divideObjects)) res
        | TokenToVal "*" (Infix (1, multObjects)) res
        | TokenToVal "+" (Infix (2, addObjects)) res 
        | TokenToVal "-" (Infix (2, subObjects)) res
        | TextCapture '"' res
        | TextCapture ''' res 
        | Num res ->
            let v, rem = res 
            match v with
            | Some value -> parseProgramInner rem (value :: result) currentCaptures
            | None -> parseProgramInner rem result currentCaptures
        | FreeToken ["."; " "; "("; ")"; ","; "."; "]"; "["] (token, rem) ->
            parseProgramInner rem ((Unknown token) :: result) currentCaptures
        | str -> parseProgramInner (str.Substring(1)) result currentCaptures
    let res, _, _ = parseProgramInner startText [] [] in res |> List.rev
