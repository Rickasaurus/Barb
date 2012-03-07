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

type BarbParsingException (message, index) = 
    inherit Exception (message)
    member t.Index = index

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
    let isnumchar c = c >= '0' && c <= '9' 
    let sb = new StringBuilder()
    if isnumchar text.[0] || (text.[0] = '.' && text.Length >= 2 && isnumchar text.[1]) then
        let rec inner = 
            function
            | i, dot when i >= text.Length -> i, dot
            | i, true when text.[i] = '.' -> i, true
            | i, false when text.[i] = '.' -> sb.Append(text.[i]) |> ignore; inner (i+1, true)
            | i, dotSeen when isnumchar text.[i] -> sb.Append(text.[i]) |> ignore; inner (i+1, dotSeen)
            | i, dot -> i, dot
        let resi, dot = inner (0, false)
        let tokenStr = 
            let resultStr = sb.ToString()
            if dot then match Double.TryParse(resultStr) with | true, num -> Obj num | _ -> Obj resultStr
            else match Int64.TryParse(resultStr) with | true, num -> Obj num | _ -> Obj resultStr
        let rest = text.Subwindow(resi)
        Some (Some (tokenStr), rest)
    else None
        
let (|CaptureString|_|) (b: char) (text: StringWindow) = 
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

let (|CaptureUnknown|_|) (endTokens: string list) (text: StringWindow) =
    let endIndices = 
        endTokens 
        |> List.map (fun e -> text.IndexOf e)
        |> List.filter (fun i -> i >= 0)
    match endIndices with
    | [] -> Some (Some (Unknown (text.ToString())), StringWindow("", 0))
    | list -> 
        let index = list |> List.min 
        let tokenText = text.Substring(0, index)
        let remainder = text.Subwindow(index)
        if index > 0 then 
            Some (Some (Unknown tokenText), remainder) 
        else None

let generateBind : ExprTypes list -> ExprTypes = 
    function 
    | SubExpression([Unknown(name)]) :: expr :: [] -> Binding (name, expr) 
    | list -> failwith (sprintf "Incorrect binding syntax: %A" list)

let generateLambda : ExprTypes list -> ExprTypes = 
    function 
    | SubExpression(names) :: contents :: [] ->
        let prms = names |> List.map (function | Unknown n -> n | other -> failwith (sprintf "Unexpected construct in lambda argument list: %A" other))
        Lambda (prms, List.empty, contents) 
    | list -> failwith (sprintf "Incorrect lambda binding syntax: %A" list)

let generateIfThenElse =
    function
    | SubExpression(ifexpr) :: SubExpression(thenexpr) :: SubExpression(elseexpr) :: [] -> IfThenElse (ifexpr, thenexpr, elseexpr)
    | list -> failwith (sprintf "Incorrect if-then-else syntax: %A" list)

let generateNumIterator = 
    function
    |  SubExpression(starte) :: SubExpression(ende) :: [] -> 
        Generator (SubExpression(starte), Obj(box 1L), SubExpression(ende))
    | SubExpression(starte) :: SubExpression(inc) :: SubExpression(ende) :: [] -> 
        Generator (SubExpression(starte), SubExpression(inc), SubExpression(ende))        
    | list -> failwith (sprintf "Incorrect generator syntax: %A" list)

let whitespaceVocabulary = [" "; "\t"; "\r"; "\n"] 

let allExpressionTypes = 
    [
        { Pattern = [Open; RCap ","; Open];                         Func = (fun exprs -> Tuple (exprs |> List.toArray)) }
        { Pattern = [Open; SCap "=>"; Open];                        Func = generateLambda }
        { Pattern = [SCap "fun"; SCap "->"; Open];                  Func = generateLambda }
        { Pattern = [SCap "if"; SCap "then"; SCap "else"; Open];    Func = generateIfThenElse }
        { Pattern = [SCap "("; SCap ")"];                           Func = (function | [SubExpression([])] -> Unit | exprs -> SubExpression exprs) }
        { Pattern = [SCap "{"; RCap ".."; SCap "}"];                Func = generateNumIterator }
        { Pattern = [SCap "["; SCap "]"];                           Func = (fun exprs -> IndexArgs <| SubExpression exprs) }
        { Pattern = [SCap "let"; SCap "="; SCap "in"];              Func = generateBind }
        { Pattern = [SCap "var"; SCap "="; SCap "in"];              Func = generateBind }
    ]

let allSimpleMappings = 
    [
        ["."], Invoke
        ["()"], Unit
        ["new"], New
        ["null"], Obj null
        ["true"], Obj true
        ["false"], Obj false
        ["=="; "="], Infix (3, objectsEqual)
        ["<>"; "!="], Infix (3, objectsNotEqual)
        [">="], Infix (3, compareObjects (>=))
        ["<="], Infix (3, compareObjects (<=))
        [">"], Infix (3, compareObjects (>))
        ["<"], Infix (3, compareObjects (<))
        ["!"; "not"], Prefix notOp
        ["&"; "&&"; "and"], Infix (4, andOp)
        ["|"; "||"; "or"], Infix (4, orOp)
        ["\\/"], Infix (2, unionObjects)
        ["/\\"], Infix (2, intersectObjects)
        ["/?\\"], Infix (2, doObjectsIntersect)
        ["/"], Infix (1, divideObjects)
        ["*"], Infix (1, multObjects)
        ["+"], Infix (2, addObjects)
        ["-"], Infix (2, subObjects)
    ]

let endUnknownChars = 
    seq {
        for c in whitespaceVocabulary do
            yield c.[0]
        for e in allExpressionTypes do
            for token in e.Pattern do
                match token with
                | Open -> ()
                | SCap (s) 
                | RCap (s) -> yield s.[0]
        for tokens, expr in allSimpleMappings do
            for token in tokens do
                yield token.[0]
    } 
    |> Seq.filter (fun c -> not (c >= 'A' && c <= 'Z') && not (c >= 'a' && c <= 'z'))
    |> Set.ofSeq
    |> Set.toList
    |> List.map (fun c -> string c)    

let (|Skip|_|) (skipStrs: string list) (text: StringWindow) =
    skipStrs 
    |> List.tryFind (fun sstr -> text.StartsWith(sstr))
    |> Option.map (fun m -> None, text.Subwindow(m.Length))

let (|MapSymbol|_|) (text: StringWindow) =
    let matches, str = 
        [
            for matchStrs, expr in allSimpleMappings do
                for matchStr in matchStrs do
                    if text.StartsWith(matchStr) then yield matchStr, expr
        ] |> List.allMaxBy (fun (m, expr) -> m.Length)
    match matches with
    | [] -> None
    | [(matched, expr)] -> Some (Some(expr), text.Subwindow(matched.Length))
    | _ -> failwith (sprintf "Ambiguous symbol match: %A" matches)

let (|NewExpression|_|) (typesStack: SubexpressionType list) (text: StringWindow) =
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
    | []-> None
    | [(mtext, subexprtype)] -> Some (subexprtype, text.Subwindow(mtext.Length))
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
                | Some (ancestor) -> 
                    match ancestor.Pattern with
                    | ((SCap h) :: rest) 
                    | ((RCap h) :: rest) when text.StartsWith(h) -> Some (current, text)
                    | _ -> None
                | None when text.Length = 0 -> Some (current, text)
                | None -> None
            | _ -> None
        | _ -> None

let parseProgram (startText: string) = 
    let rec parseProgramInner (str: StringWindow) (result: ExprTypes list) (currentCaptures: SubexpressionType list) : (StringWindow * ExprTypes) =
        try
            match result with
            | SubExpression cSubExpr :: rSubExprs -> 
                match str with
                | FinishOpenExpression currentCaptures (subtype, crem) ->
                    let innerResult = SubExpression (cSubExpr |> List.rev) :: rSubExprs  
                    let value = innerResult |> List.rev |> subtype.Func in 
                        crem, value         
                | _ when str.Length = 0 -> str, SubExpression (SubExpression (cSubExpr |> List.rev) :: rSubExprs)            
                | OngoingExpression currentCaptures (captures, crem) ->
                    match captures with
                    // Expression is Finished
                    | { Pattern = []; Func = func } :: parents -> 
                        let innerResult = SubExpression (cSubExpr |> List.rev) :: rSubExprs  
                        let value = innerResult |> List.rev |> func in 
                            crem, value   
                    // Expression Continues
                    | { Pattern = h :: rest; Func = _ } :: parents -> parseProgramInner crem (SubExpression [] :: SubExpression (cSubExpr |> List.rev) :: rSubExprs) captures
                    | [] -> failwith "Unexpected output from OngoingExpression"
                | RefineOpenExpression currentCaptures (subtype, crem) ->
                    // Mid-Expression we've realized we're actually in a different kind.
                    let rem, value = parseProgramInner crem (SubExpression [] :: SubExpression (cSubExpr |> List.rev) :: []) (subtype :: currentCaptures)               
                    parseProgramInner rem (SubExpression ([value]) :: rSubExprs) currentCaptures    
                | NewExpression currentCaptures (subtype, crem) ->
                    let rem, value = parseProgramInner crem [SubExpression []] (subtype :: currentCaptures)               
                    parseProgramInner rem (SubExpression (value :: cSubExpr) :: rSubExprs) currentCaptures
                | Skip whitespaceVocabulary res
                | Num res
                | MapSymbol res
                | CaptureString '"' res
                | CaptureString ''' res 
                | CaptureUnknown endUnknownChars res ->
                    let v, rem = res 
                    match v with
                    | Some value -> parseProgramInner rem (SubExpression (value :: cSubExpr) :: rSubExprs) currentCaptures
                    | None -> parseProgramInner rem result currentCaptures
                | str -> parseProgramInner (str.Subwindow(1)) result currentCaptures
            | _ -> failwith "Expected a SubExpression"
        with ex -> raise <| new BarbParsingException(ex.Message, str.Length)
    let _, res = parseProgramInner (StringWindow(startText, 0)) [SubExpression []] [] in res