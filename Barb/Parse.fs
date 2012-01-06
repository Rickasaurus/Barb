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

let (|Skip|_|) (mStr: string) (text: StringWindow) =
    if text.StartsWith(mStr) then 
        Some (None, text.Subwindow(mStr.Length))
    else None

let (|StrToVal|_|) (mStr: string) (result: ExprTypes) (text: StringWindow) =
    if text.StartsWith(mStr) then 
        Some (Some(result), text.Subwindow(mStr.Length))
    else None

let (|StrsToVal|_|) (mStrs: string list) (result: ExprTypes) (text: StringWindow) =
    match mStrs |> List.tryFind (fun mStr -> text.StartsWith(mStr)) with
    | Some (matched) -> Some (Some(result), text.Subwindow(matched.Length))
    | None -> None

type DelimType =
    | Open
    | SCap of string
    | RCap of string

type SubexpressionType = 
    {
        Pattern: DelimType list
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

let generateBind : ExprTypes list -> ExprTypes = 
    function 
    | h :: SubExpression([Unknown(name)]) :: [] -> Binding (name, h) 
    | list -> failwith (sprintf "Incorrect binding syntax: %A" list)

let generateLambda : ExprTypes list -> ExprTypes = 
    function 
    | contents :: SubExpression(names) :: [] ->
        let prms = names |> List.map (function | Unknown n -> n | other -> failwith (sprintf "Unexpected construct in lambda argument list: %A" other))
        Lambda (prms, List.empty, contents) 
    | list -> failwith (sprintf "Incorrect lambda binding syntax: %A" list)

let generateIfThenElse =
    function
    | SubExpression(elseexpr) :: SubExpression(thenexpr) :: SubExpression(ifexpr) :: [] -> IfThenElse (ifexpr, thenexpr, elseexpr)
    | list -> failwith (sprintf "Incorrect if-then-else syntax: %A" list)

let generateNumIterator = 
    function
    | SubExpression(ende) :: SubExpression(starte) :: [] -> 
        Generator (SubExpression(starte), Obj(box 1L), SubExpression(ende))
    | SubExpression(ende) :: SubExpression(inc) :: SubExpression(starte) :: [] -> 
        Generator (SubExpression(starte), SubExpression(inc), SubExpression(ende))        
    | list -> failwith (sprintf "Incorrect generator syntax: %A" list)

//let rec extractUnknowns exprs unknowns =
//    match exprs with
//    | [] -> unknowns
//    | Unknown u :: rest -> extract rest [u :: unknowns]
//    | _ :: rest -> extractUnknowns rest unknowns

//let rec extractTopBindings exprs unknowns =
//    match exprs with
//    | [] -> unknowns
//    | Binding b :: rest -> extract rest [b :: unknowns]
//    | _ :: rest -> extractUnknowns rest unknowns

//let generateLoop = 
//    function
//    | SubExpression(contents) :: SubExpression(predicate) :: SubExpression([]) :: [] ->
//        let topBindings = extractTopBindings contents
//    | list -> failwith (sprintf "Incorrect while syntax %A" list)

let allExpressionTypes = 
    [
        { Pattern = [Open; RCap ","; Open];                         Func = (fun exprs -> Tuple (exprs |> List.rev |> List.toArray)) }
        { Pattern = [Open; SCap "=>"; Open];                        Func = generateLambda }
        { Pattern = [SCap "fun"; SCap "->"; Open];                  Func = generateLambda }
        { Pattern = [SCap "if"; SCap "then"; SCap "else"; Open];    Func = generateIfThenElse }
        { Pattern = [SCap "("; SCap ")"];                           Func = (function | [SubExpression([])] -> Unit | exprs -> SubExpression exprs) }
        { Pattern = [SCap "{"; RCap ".."; SCap "}"];                Func = generateNumIterator }
        { Pattern = [SCap "["; SCap "]"];                           Func = (fun exprs -> IndexArgs <| SubExpression exprs) }
        { Pattern = [SCap "let"; SCap "="; SCap "in"];              Func = generateBind }
        { Pattern = [SCap "var"; SCap "="; SCap "in"];              Func = generateBind }
//        { Pattern = [SCap "loop"; SCap "{"; SCap "}"];              Func = generateLoop }
    ]

let (|NewExpression|_|) (text: StringWindow) =
    let matches, str = 
        [ 
            for ct in allExpressionTypes do 
                match ct.Pattern with
                | (SCap h) :: rest when text.StartsWith(h) -> yield h, { ct with Pattern = rest }
                | (RCap h) :: rest when text.StartsWith(h) -> yield h, ct
                | _ -> ()    
        ] |> List.allMaxBy (fun (m, rest) -> m.Length)
    match matches with
    | [] -> None
    | [(mtext, subexprtype)] -> Some (subexprtype, text.Subwindow(mtext.Length))
    | _ -> failwith (sprintf "Ambiguous expression match: %A" matches)

let (|OngoingExpression|_|) (typesStack: SubexpressionType list) (text: StringWindow) =
        match typesStack with
        | current :: parents -> 
            match current.Pattern with
            | (SCap h) :: rest when text.StartsWith(h) -> Some (h, { current with Pattern = rest } :: parents)
            | (RCap h) :: rest when text.StartsWith(h) -> Some (h, current :: parents)
            | (RCap _) :: (SCap h) :: rest when text.StartsWith(h) -> Some (h, { current with Pattern = rest } :: parents)
            | _ -> None
            |> Option.map (fun (mtext, subexp) -> subexp, text.Subwindow(mtext.Length))
        | _ -> None

let (|RefineOpenExpression|_|) (typesStack: SubexpressionType list) (text: StringWindow) =
    let matches, str = 
        [ 
            for ct in allExpressionTypes do 
                match ct.Pattern with
                | Open :: (SCap h) :: rest when text.StartsWith(h) -> yield h, { ct with Pattern = rest } 
                | Open :: (RCap h) :: rest when text.StartsWith(h) -> yield h, { ct with Pattern = (RCap h) :: rest }
                | _ -> ()    
        ] |> List.allMaxBy (fun (m, rest) -> m.Length)   
    match matches with
    | [] -> None
    | [(mtext, subexprtype)] -> Some (subexprtype :: typesStack, text.Subwindow(mtext.Length))
    | _ -> failwith (sprintf "Ambiguous open expression match: %A" matches)    

let rec findClosed (typesStack: SubexpressionType list) = 
    match typesStack with
    | [] -> None
    | {Pattern = (Open :: _); Func = _} :: rest -> findClosed rest
    | other :: rest -> Some other

// Note, don't move the text pointer when finishing an open expression, so that the parent expression is closed.
let (|FinishOpenExpression|_|) (typesStack: SubexpressionType list) (text: StringWindow) =
        match typesStack with
        | current :: rest -> 
            match current.Pattern with
            | (Open) :: [] 
            | (RCap _) :: Open :: [] -> 
                match findClosed rest with
                | Some (parent) -> 
                    match parent.Pattern with
                    | ((SCap h) :: rest) 
                    | ((RCap h) :: rest) when text.StartsWith(h) -> Some (current, text)
                    | _ -> None
                | None when text.Length = 0 -> Some (current, text)
                | None -> None
            | _ -> None
        | _ -> None

let parseProgram (startText: string) = 
    let rec parseProgramInner (str: StringWindow) (result: ExprTypes list) (currentCaptures: SubexpressionType list) =
        match str with
        | FinishOpenExpression currentCaptures (subtype, crem) ->
            match result with
            | [] -> crem, [SubExpression result], currentCaptures
            | results -> crem, [SubExpression (results |> List.rev)], currentCaptures 
        | _ when str.Length = 0 -> str, result, currentCaptures
        | OngoingExpression currentCaptures (captures, crem) ->
            match captures with
            | { Pattern = []; Func = _ } :: parents -> crem, [SubExpression (result |> List.rev)], captures
            | { Pattern = h :: rest; Func = _ } :: parents ->
                let rem, subExprs, captures = parseProgramInner crem [] captures
                rem, SubExpression (result |> List.rev) :: subExprs, captures 
            | _ -> failwith "OngoingExpression returned no expression"
        | NewExpression (subtype, crem) ->
            let rem, subExprs, capture = parseProgramInner crem [] (subtype :: currentCaptures)
            let resolvedCap = 
                match capture with            
                | h :: rest -> h
                | _ -> failwith "Unexpected end of subexpression"
            let value = subExprs |> List.rev |> resolvedCap.Func 
            parseProgramInner rem (value :: result) currentCaptures
        | RefineOpenExpression currentCaptures (captures, crem) ->
            let rrem, rSubExprs, rCaptures = parseProgramInner crem [] captures
            rrem, SubExpression (result |> List.rev) :: rSubExprs, captures 
        | Skip " " res
        | Skip "\t" res
        | Skip "\r" res
        | Skip "\n" res
        | StrToVal "." Invoke res
        | StrToVal "()" Unit res
        | StrToVal "null" (Obj null) res
        | StrToVal "true" (Obj true) res 
        | StrToVal "false" (Obj false) res 
        | StrsToVal ["=="; "="] (Infix (3, objectsEqual)) res 
        | StrsToVal ["<>"; "!="] (Infix (3, objectsNotEqual)) res
        | StrToVal ">=" (Infix (3, compareObjects (>=))) res
        | StrToVal "<=" (Infix (3, compareObjects (<=))) res
        | StrToVal ">" (Infix (3, compareObjects (>))) res
        | StrToVal "<" (Infix (3, compareObjects (<))) res 
        | StrsToVal ["!"; "not"] (Prefix notOp) res
        | StrsToVal ["&"; "&&"; "and"] (Infix (4, andOp)) res
        | StrsToVal ["|"; "||"; "or"] (Infix (4, orOp)) res 
        | StrToVal "/" (Infix (1, divideObjects)) res
        | StrToVal "*" (Infix (1, multObjects)) res
        | StrToVal "+" (Infix (2, addObjects)) res 
        | StrToVal "-" (Infix (2, subObjects)) res
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
    let _, res, _ = parseProgramInner (StringWindow(startText, 0)) [] [] in res |> List.rev
