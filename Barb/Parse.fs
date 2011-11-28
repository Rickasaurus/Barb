module Barb.Parse

// TODO: 
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

type StringWindow =
    struct
        // These are mutable to prevent property generation.
        val mutable Text: string 
        val mutable Offset: int
        new(text: string, offset: int) = { Text = text; Offset = offset }
    end
    with 
        member t.StartsWith (str: string) =            
            let text = t.Text
            let offset = t.Offset
            let textLen = t.Length
            let rec matches i res =
                if i > textLen - 1 then false
                else
                    let newres = res && (text.[offset + i] = str.[i])
                    match i, newres with | 0, _ -> newres | _, false -> newres | _ -> matches (i - 1) newres
            matches (str.Length - 1) true
        member t.Length = t.Text.Length - t.Offset
        member t.Subwindow start = StringWindow (t.Text, t.Offset + start)
        member t.Substring (index, len) = t.Text.Substring(t.Offset + index, len)
        member t.IndexOf (pattern: string) = max (t.Text.IndexOf(pattern, t.Offset) - t.Offset) -1
        member t.Item with get(x) = t.Text.[x + t.Offset]
        override t.ToString () = t.Text.Substring(t.Offset) 

let (|SkipToken|_|) (mStr: string) (text: StringWindow) =
    if text.StartsWith(mStr) then 
        Some (None, text.Subwindow(mStr.Length))
    else None

let (|TokenToVal|_|) (mStr: string) (result: ExprTypes) (text: StringWindow) =
    if text.StartsWith(mStr) then 
        Some (Some(result), text.Subwindow(mStr.Length))
    else None

let (|TokensToVal|_|) (mStrs: string list) (result: ExprTypes) (text: StringWindow) =
    match mStrs |> List.tryFind (fun mStr -> text.StartsWith(mStr)) with
    | Some (matched) -> Some (Some(result), text.Subwindow(matched.Length))
    | None -> None

type DelimType =
    | SCapture of string
    | RCapture of string

type CaptureParams = 
    {
        Begin: string
        Delims: DelimType list
        End: string        
        Func: ExprTypes list -> ExprTypes
    }

open System.Numerics

let whitespace = [| " "; "\r"; "\n"; "\t"; |]

let (|Num|_|) (text: StringWindow) =
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
            if dot then match Double.TryParse(resultStr) with | true, num -> Obj num | _ -> Obj resultStr
            else match Int64.TryParse(resultStr) with | true, num -> Obj num | _ -> Obj resultStr
        let rest = text.Subwindow(resi)
        Some (Some (tokenStr), rest)
    else None
        
let (|TextCapture|_|) (b: char) (text: StringWindow) = 
    if text.[0] = b then 
        let sb = new StringBuilder()
        let rec findSafeIndex i =
            if i >= text.Length then failwith "Quotes not matched"
            elif text.[i] = '\\' then findSafeIndex(i + 1)
            elif text.[i] = b && text.[i - 1] <> '\\' then 
                let tokenStr = Some (Obj (sb.ToString() :> obj))
                let rest = text.Subwindow(i + 1)
                Some (tokenStr, rest)
            else sb.Append(text.[i]) |> ignore; findSafeIndex (i + 1)
        findSafeIndex 1
    else None  

let (|FreeToken|_|) (endTokens: string list) (text: StringWindow) =
    let endIndices = 
        endTokens 
        |> List.map (fun e -> text.IndexOf e)
        |> List.filter (fun i -> i >= 0)
    match endIndices with
    | [] -> Some (text.ToString(), StringWindow("", 0))
    | list -> 
        let index = list |> List.min 
        let tokenText = text.Substring(0, index)
        let remainder = text.Subwindow(index)
        if index > 0 then Some (tokenText, remainder) 
        else None

let generateBind = 
    function 
    | h :: SubExpression([Unknown(name)]) :: [] -> Binding (name, h) 
    | list -> failwith (sprintf "Incorrect binding syntax: %A" list)

let generateLambda = 
    function 
    | h :: SubExpression(names) :: SubExpression([]) :: [] ->
        let prms = names |> List.map (function | Unknown n -> n | other -> failwith (sprintf "Unexpected construct in lambda argument list: %A" other))
        Lambda (prms, List.empty, h) 
    | list -> failwith (sprintf "Incorrect lambda binding syntax: %A" list)

let generateIfThenElse =
    function
    | SubExpression(elseexpr) :: SubExpression(thenexpr) :: SubExpression(ifexpr) :: SubExpression([]) :: [] -> IfThenElse (ifexpr, thenexpr, elseexpr)
    | list -> failwith (sprintf "Incorrect if-then-else syntax: %A" list)

let generateNumIterator = 
    function
    | SubExpression(ende) :: SubExpression(starte) :: [] -> 
        Generator (SubExpression(starte), Obj(box 1L), SubExpression(ende))
    | SubExpression(ende) :: SubExpression(inc) :: SubExpression(starte) :: [] -> 
        Generator (SubExpression(starte), SubExpression(inc), SubExpression(ende))        
    | list -> failwith (sprintf "Incorrect generator syntax: %A" list)

let captureTypes = 
    [
        { Begin = "(";   Delims = [];                                                 End = ")";  Func = (function | [] -> Unit | exprs -> SubExpression exprs) }
        { Begin = "(";   Delims = [RCapture ","];                                     End = ")";  Func = (fun exprs -> Tuple exprs) }
        { Begin = "(";   Delims = [SCapture "fun"; SCapture "->"];                    End = ")";  Func = generateLambda }
        { Begin = "(";   Delims = [SCapture "=>"];                                    End = ")";  Func = generateLambda }
        { Begin = "(";   Delims = [SCapture "if"; SCapture "then"; SCapture "else"];  End = ")";  Func = generateIfThenElse }
        { Begin = "(";   Delims = [RCapture ".."];                                    End = ")";  Func = generateNumIterator }
        { Begin = "[";   Delims = [];                                                 End = "]";  Func = (fun exprs -> IndexArgs <| SubExpression exprs) }
        { Begin = "let"; Delims = [SCapture "="];                                     End = "in"; Func = generateBind }
        { Begin = "var"; Delims = [SCapture "="];                                     End = "in"; Func = generateBind }
    ]

let (|CaptureDelim|_|) currentCaptures (text: StringWindow) =
    let matches, delimLen = 
        [ 
            for cc in currentCaptures do
                match cc.Delims with
                | (SCapture delim) :: dt when text.StartsWith delim -> yield { cc with Delims = dt }, delim
                | (RCapture delim) :: dt when text.StartsWith delim -> yield cc, delim
                | (RCapture _) :: RCapture delim :: dt when text.StartsWith delim -> yield { cc with Delims = RCapture delim :: dt }, delim
                | (RCapture _) :: SCapture delim :: dt when text.StartsWith delim -> yield { cc with Delims = dt }, delim  
                | _ -> ()          
        ] 
        |> List.allMaxBy (fun (cc, matchedDelim) -> matchedDelim.Length)
        |> (fun (matches, delimlen) -> matches |> List.map (fun (cc, txt) -> cc), delimlen) 
    match matches with
    | [] -> None
    | onecap :: [] -> Some ([onecap], text.Subwindow(delimLen))
    | caps -> Some (caps, text.Subwindow(delimLen))

let (|CaptureEnd|_|) currentCaptures (text: StringWindow) =
    let matches, str = [ for cc in currentCaptures do if text.StartsWith cc.End then yield cc ] |> List.allMaxBy (fun ct -> ct.End.Length) 
    match matches with
    | [] -> None
    | onecap :: [] -> Some ([onecap], text.Subwindow(onecap.End.Length))
    | caps -> Some (caps, text.Subwindow(str))

let (|CaptureBegin|_|) (text: StringWindow) =
    let matches, str = [ for ct in captureTypes do if text.StartsWith ct.Begin then yield ct ] |> List.allMaxBy (fun ct -> ct.Begin.Length)
    match matches with
    | [] -> None
    | onecap :: [] -> Some ([onecap], text.Subwindow(onecap.Begin.Length))
    | caps -> Some (caps, text.Subwindow(str))

let parseProgram (startText: string) = 
    let rec parseProgramInner (str: StringWindow) (result: ExprTypes list) (currentCaptures: CaptureParams list) =
        match str with
        | _ when str.Length = 0 -> result, str, currentCaptures
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
        | SkipToken "\t" res
        | SkipToken "\r" res
        | SkipToken "\n" res
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
        | FreeToken ["."; " "; "("; ")"; ","; "."; "]"; "["; "\r"; "\n"; "\t"] (token, rem) ->
            parseProgramInner rem ((Unknown token) :: result) currentCaptures
        | str -> parseProgramInner (str.Subwindow(1)) result currentCaptures
    let res, _, _ = parseProgramInner (StringWindow(startText, 0)) [] [] in res |> List.rev
