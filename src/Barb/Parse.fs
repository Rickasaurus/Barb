module internal Barb.Parse

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

#if DEBUG
open Checked
#endif

type BarbParsingException (message, offset, length) =
    inherit BarbException (message, offset, length)

type internal StringWindow =
    struct
        // These are mutable to prevent property generation.
        val mutable Text: string 
        val mutable Offset: uint32
        new(text: string, offset: uint32) = { Text = text; Offset = offset }
    end
    with 
        member t.StartsWith (str: string) =            
            let text = t.Text
            let offset = t.Offset
            let textLen = t.Length
            let rec matches (i: uint32) res =
                if i >= textLen then false
                else
                    let newres = res && (text.[int <| offset + i] = str.[int i])
                    match i, newres with | 0u, _ -> newres | _, false -> newres | _ -> matches (i - 1u) newres
            matches (uint32 str.Length - 1u) true
        member t.Length = (uint32 t.Text.Length) - t.Offset
        member t.Subwindow start = StringWindow (t.Text, t.Offset + start)
        member t.Substring (index, len) = t.Text.Substring(int <| t.Offset + index, int len)
        member t.IndexOf (pattern: string) : int = max (t.Text.IndexOf(pattern, int t.Offset) - int t.Offset) -1 
        member t.Item with get(x: uint32) = t.Text.[int (x + t.Offset)]
        override t.ToString () = t.Text.Substring(int t.Offset) 


type MatchReturn = (ExprTypes option * StringWindow) option

type DelimType =
    | Open
    | SCap of string
    | RCap of string

type SubexpressionType = 
    {
        Pattern: DelimType list
        Func: ExprRep list -> ExprRep
    }

type SubexpressionStatus = 
    // Resolved Subexpression (only one possible type)
    | RSubExpr of SubexpressionType
    // Unresolved subexpression (more than one possible type)
    | USubExpr of SubexpressionType list

type SubexpressionAndOffset = SubexpressionStatus * uint32

open System.Numerics

let whitespace = [| " "; "\r"; "\n"; "\t"; |]    

let (|Num|_|) (text: StringWindow) : MatchReturn =
    let isnumchar c = c >= '0' && c <= '9' 
    let isnegative c = c = '-'
    let textStartAt = text.Offset
    let sb = new StringBuilder()
    if isnumchar text.[0u]  
       || (text.[0u] = '.' && text.Length >= 2u && isnumchar text.[1u])  
       || (isnegative text.[0u] && text.Length >= 2u && isnumchar text.[1u])
       then
        let rec inner = 
            function
            | i, dot when i >= text.Length -> i, dot
            | i, true when text.[i] = '.' -> i, true
            | i, false when text.[i] = '.' -> sb.Append(text.[i]) |> ignore; inner (i+1u, true)
            | i, dotSeen when isnumchar text.[i] -> sb.Append(text.[i]) |> ignore; inner (i+1u, dotSeen)
            | i, dot -> i, dot
        let isNegative = isnegative text.[0u]
        let resi, dot = 
            if isnegative text.[0u] then 
                sb.Append(text.[0u]) |> ignore
                inner (1u, false)
            else inner (0u, false)
        let tokenStr = 
            let resultStr = sb.ToString()
            if dot then match Double.TryParse(resultStr) with 
                              | true, num -> Obj num 
                              | _ -> Obj resultStr
            else match Int64.TryParse(resultStr) with | true, num -> Obj num | _ -> Obj resultStr
        let rest = text.Subwindow(resi)
        Some (Some (tokenStr), rest)
    else None
        
let (|CaptureString|_|) (b: char) (text: StringWindow) : MatchReturn = 
    if text.[0u] = b then 
        let sb = new StringBuilder()
        let rec findSafeIndex i =
            if i >= text.Length then failwith "Quotes not matched"
            elif text.[i] = '\\' then findSafeIndex(i + 1u)
            elif text.[i] = b && text.[i - 1u] <> '\\' then 
                let tokenStr = Some (Obj (sb.ToString()))
                let rest = text.Subwindow(i + 1u)
                Some (tokenStr, rest)
            else sb.Append(text.[i]) |> ignore; findSafeIndex (i + 1u)
        findSafeIndex 1u
    else None  

let (|CaptureChar|_|) (b: char) (text: StringWindow) : MatchReturn = 
    if text.[0u] = b then 
        if text.[2u] <> b then failwith "Incorrect character syntax."
        Some (Some <| Obj text.[1u], text.Subwindow(3u))
    else None  


let (|CaptureUnknown|_|) (endTokens: string list) (text: StringWindow) : MatchReturn =
    let endIndices = 
        endTokens 
        |> List.map (fun e -> text.IndexOf e)
        |> List.filter (fun i -> i >= 0)
    match endIndices with
    | [] -> 
        Some (Some (Unknown (text.ToString())), text.Subwindow(text.Length))
    | list -> 
        let index = list |> List.min |> uint32
        let tokenText = text.Substring(0u, index)
        let remainder = text.Subwindow(index)
        if index > 0u then 
            Some (Some (Unknown tokenText), remainder) 
        else None

let makeTuple (exprs: ExprRep list) : ExprRep = 
    let offset, length = exprRepListOffsetLength exprs in 
        { Offset = offset; Length = length; Expr = Tuple (exprs |> List.toArray) }

let makeArray (exprs: ExprRep list) : ExprRep = 
    let offset, length = exprRepListOffsetLength exprs in 
        match exprs with
        | { Expr = SubExpression s } :: [] when s.Length = 0 -> 
            { Offset = offset; Length = length; Expr = ArrayBuilder [||] }    
        | exprs ->
            { Offset = offset; Length = length; Expr = ArrayBuilder (exprs |> List.toArray) }    
    
let makeLambda (exprs: ExprRep list) : ExprRep = 
    match exprs with 
    | { Expr = SubExpression(names) } :: contents :: [] ->
        let offset, length = exprRepListOffsetLength exprs
        let prms = names |> List.map (function | { Expr = Unknown n } -> n | other -> failwithf "Unexpected construct in lambda argument list: %A" other)
        { Offset = offset; Length = length; Expr = Lambda { Params = prms; Bindings = Map.empty; Contents = contents } }
    | list -> failwithf "Incorrect lambda binding syntax: %A" list

let makeIfThenElse (exprs: ExprRep list) : ExprRep = 
    match exprs with
    | ({ Expr = SubExpression(ifexpr)   } & ifRep)   :: 
      ({ Expr = SubExpression(thenexpr) } & thenRep) ::
      ({ Expr = SubExpression(elseexpr) } & elseRep) :: [] -> 
        let offset, length = exprRepListOffsetLength exprs
        { Offset = offset; Length = length; Expr = IfThenElse (ifRep, thenRep, elseRep) }
    | list -> failwithf "Incorrect if-then-else syntax: %A" list

let makeUnitOrSubExpression: ExprRep list -> ExprRep =  
    function
    // Unit
    | [{Offset = offset; Length = length; Expr = SubExpression([]) }] -> { Offset = offset; Length = length; Expr = Unit }
    // Subexpression
    | exprs -> let offset, length = exprRepListOffsetLength exprs
               { Offset = offset; Length = length; Expr = SubExpression exprs } 


let makeNumIterator: ExprRep list -> ExprRep =  
    function
    | startRep :: endRep :: []
      & { Expr = SubExpression(_) } :: { Expr = SubExpression(_) } :: [] -> 
        let length = (endRep.Offset + endRep.Length) - startRep.Offset
        // Report error for the whole generator if the middle is somehow wrong when missing
        let missingVal = { Offset = startRep.Offset; Length = length; Expr = Obj 1L } 
        { Offset = startRep.Offset; Length = length; Expr = Generator (startRep, missingVal, endRep) }
    | startRep :: midRep :: endRep :: [] 
      & { Expr = SubExpression(_) } :: { Expr = SubExpression(_) } :: { Expr = SubExpression(_) } :: [] -> 
        let length = (endRep.Offset + endRep.Length) - startRep.Offset
        { Offset = startRep.Offset; Length = length; Expr = Generator (startRep, midRep, endRep) }
    | list -> failwith (sprintf "Incorrect generator syntax: %A" list)

let makeIndexArgs (exprs: ExprRep list) : ExprRep = 
    let offset, length = exprRepListOffsetLength exprs in 
        { Offset = offset; Length = length; Expr = IndexArgs (exprs |> List.toArray) }

let makeBind : ExprRep list -> ExprRep = 
    function
    | exprs & { Offset = offset; Length = length; Expr = SubExpression([{ Expr = Unknown(name) }]) } :: bindExpr :: scopeExpr :: [] -> 
        { Offset = offset; Length = length; Expr = BVar (name, bindExpr, scopeExpr) }
    | list -> failwith (sprintf "Incorrect binding syntax: %A" list)

let makeAnd (exprs: ExprRep list) : ExprRep = 
    match exprs with
    | firstExpr :: secondExpr :: [] ->
        let offset, length = exprRepListOffsetLength exprs
        { Offset = offset; Length = length; Expr = And (firstExpr, secondExpr) }
    | list -> failwith (sprintf "Incorrect and syntax: %A" list)

let makeOr (exprs: ExprRep list) : ExprRep = 
    match exprs with
    | firstExpr :: secondExpr :: [] ->
        let offset, length = exprRepListOffsetLength exprs
        { Offset = offset; Length = length; Expr = Or (firstExpr, secondExpr) }
    | list -> failwith (sprintf "Incorrect and syntax: %A" list)


let allExpressionTypes = 
    [
        { Pattern = [Open; RCap ","; Open];                         Func = makeTuple }
        { Pattern = [SCap "[|"; RCap ";"; SCap "|]"];               Func = makeArray } 
        { Pattern = [Open; SCap "=>"; Open];                        Func = makeLambda }
        { Pattern = [SCap "fun"; SCap "->"; Open];                  Func = makeLambda }
        { Pattern = [SCap "if"; SCap "then"; SCap "else"; Open];    Func = makeIfThenElse }
        { Pattern = [SCap "("; SCap ")"];                           Func = makeUnitOrSubExpression }
        { Pattern = [SCap "{"; RCap ".."; SCap "}"];                Func = makeNumIterator }
        { Pattern = [SCap "["; RCap ","; SCap "]"];                 Func = makeIndexArgs }
//        { Pattern = [SCap "let"; SCap "="; Open];                   Func = makeBind }
        { Pattern = [SCap "let"; SCap "="; SCap "in"; Open];        Func = makeBind }
//        { Pattern = [SCap "var"; SCap "="; Open];                   Func = makeBind }
        { Pattern = [SCap "var"; SCap "="; SCap "in"; Open];        Func = makeBind }
        { Pattern = [Open; SCap "and"; Open];                       Func = makeAnd }
        { Pattern = [Open; SCap "&&"; Open];                        Func = makeAnd }
        { Pattern = [Open; SCap "or"; Open];                        Func = makeOr }
        { Pattern = [Open; SCap "||"; Open];                        Func = makeOr }
    ]

let allSimpleMappings = 
    [
        ["."], fun () -> Invoke
        ["()"], fun () -> Unit
        ["new"], fun () -> New
        ["null"], fun () -> Obj null
        ["true"], fun () -> Obj true
        ["false"], fun () -> Obj false
        ["(="], fun () -> Infix (2, isSubsetOf)
        ["=)"], fun () -> Infix (2, isSupersetOf)
        ["=="; "="], fun () -> Infix (3, objectsEqual)
        ["<>"; "!="], fun () -> Infix (3, objectsNotEqual)
        [">="], fun () -> Infix (3, compareObjects (>=))
        ["<="], fun () -> Infix (3, compareObjects (<=))
        [">"], fun () -> Infix (3, compareObjects (>))
        ["<"], fun () -> Infix (3, compareObjects (<))
        ["!"; "not"], fun () -> Prefix notOp
        ["|"; "|||"], fun () -> Infix (2, bitwiseOrObjects ())
        ["&"; "&&&"], fun () -> Infix (2, bitwiseAndObjects ())
        ["\\/"], fun () -> Infix (2, unionObjects)
        ["/\\"], fun () -> Infix (2, intersectObjects)
        ["/?\\"], fun () -> Infix (2, doObjectsIntersect)
        ["-)"], fun () -> Infix (2, leftDifference)
        ["(-"], fun () -> Infix (2, rightDifference)
        ["--"], fun () -> Infix (2, symmetricDifference)
        ["/"], fun () -> Infix (1, divideObjects ())
        ["*"], fun () -> Infix (1, multObjects ())
        ["+"], fun () -> Infix (2, addObjects ())
        ["-"], fun () -> Infix (2, subObjects ())
    ]

let whitespaceVocabulary = [" "; "\t"; "\r"; "\n"] 

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

let (|Skip|_|) (skipStrs: string list) (text: StringWindow) : MatchReturn =
    skipStrs 
    |> List.tryFind (fun sstr -> text.StartsWith(sstr))
    |> Option.map (fun m -> None, text.Subwindow(uint32 m.Length))

let getBestSimpleMappedSymbol (text: StringWindow) =
    let matches, str = 
        [
            for matchStrs, expr in allSimpleMappings do
                for matchStr in matchStrs do
                    if text.StartsWith(matchStr) then yield matchStr, expr
        ] |> List.allMaxBy (fun (m, expr) -> m.Length)
    match matches with
    | [] -> None
    | [(matched, expr)] -> Some (matched, expr, text.Subwindow(uint32 matched.Length))
    | _ -> 
        let errorText = sprintf "Ambiguous symbol match: %A" matches
        raise (new BarbParsingException(errorText, text.Offset, matches |> List.map (fun (s,e) -> s.Length) |> List.max |> uint32))    

let (|MapSymbol|_|) (text: StringWindow) : MatchReturn =
    match getBestSimpleMappedSymbol text with
    | None -> None
    | Some (matched, expr, tw) -> Some (Some(expr()), text.Subwindow(uint32 matched.Length))

let (|NewExpression|_|) (typesStack: SubexpressionAndOffset list) (text: StringWindow) =
    let antimatch = getBestSimpleMappedSymbol text
    let matches, strlen = 
        [ 
            for ct in allExpressionTypes do 
                match ct.Pattern with
                | (SCap h) :: rest when text.StartsWith(h) -> yield h, { ct with Pattern = rest }
                | (RCap h) :: rest when text.StartsWith(h) -> yield h, ct
                | _ -> ()    
        ] 
        |> List.allMaxBy (fun (m, rest) -> m.Length)
    match matches |> List.map snd, antimatch with
    | [], _ -> None
    | _, Some (sstr, _ , _) when sstr.Length > strlen -> None 
    | [subexprtype], _ -> Some (RSubExpr subexprtype, text.Subwindow(uint32 strlen))
    | subexprtypes, _ -> Some (USubExpr subexprtypes, text.Subwindow(uint32 strlen))
    | _ -> let errorText = sprintf "Ambiguous expression match: %A" matches
           raise (new BarbParsingException(errorText, text.Offset, matches |> List.map (fun (s,e) -> s.Length) |> List.max |> uint32))

    
let (|OngoingExpression|_|) (typesStack: SubexpressionAndOffset list) (text: StringWindow) =
    let (|MatchOngoingExpression|_|) (current: SubexpressionType) = 
        match current.Pattern with
        | (SCap h) :: rest when text.StartsWith(h) -> Some (h, { current with Pattern = rest })
        | (RCap h) :: rest when text.StartsWith(h) -> Some (h, current)
        | (RCap _) :: (SCap h) :: rest when text.StartsWith(h) -> Some (h, { current with Pattern = rest })
        | _ -> None

    match typesStack with
    | (RSubExpr (MatchOngoingExpression (mtext, expr)), offset) :: parents -> 
        (((RSubExpr expr, offset) :: parents), text.Subwindow(uint32 mtext.Length)) |> Some
    | (USubExpr exprs, offset) :: parents -> 
        let getMatchLen (txt: string, _) = txt.Length
        match exprs |> List.choose (function | MatchOngoingExpression (mtext, expr) -> Some (mtext, expr) | _ -> None) with
        | [] -> None
        // Refine to a single possible Subexpression type if possible
        | [(mtext, expr)] -> (((RSubExpr expr, offset) :: parents), text.Subwindow(uint32 mtext.Length)) |> Some
        // Otherwise reduce to only the longest matches
        | List.AllMaxBy (getMatchLen) (longest, strlen) -> 
            let exprs = longest |> List.map snd
            (((USubExpr exprs, offset) :: parents), text.Subwindow(uint32 strlen)) |> Some
        | failed -> failwithf "Error in implementation of List.AllMaxBy. Example: %A" failed 
    | _ -> None

let (|RefineOpenExpression|_|) (typesStack: SubexpressionAndOffset list) (text: StringWindow) =
    let antimatch = getBestSimpleMappedSymbol text
    let matches, strlen = 
        [ for ct in allExpressionTypes do 
            match ct.Pattern with
            | Open :: (SCap h) :: rest when text.StartsWith(h) -> yield h, { ct with Pattern = rest } 
            | Open :: (RCap h) :: rest when text.StartsWith(h) -> yield h, { ct with Pattern = (RCap h) :: rest }
            | _ -> ()
        ] |> List.allMaxBy (fun (m, rest) -> m.Length)   
    match matches, antimatch with
    | [], _ -> None
    | [_], Some (sstr, _ , _) when sstr.Length > strlen -> None 
    | [(mtext, subexprtype)], _ -> Some (subexprtype, text.Subwindow(uint32 strlen))
    | _ -> let errorText = sprintf "Ambiguous refinement on open expression match: %A" matches
           raise (new BarbParsingException(errorText, text.Offset, matches |> List.map (fun (s,e) -> s.Length) |> List.max |> uint32))

let rec findClosed (typesStack: SubexpressionAndOffset list) = 
    match typesStack with
    | [] -> None
    | (RSubExpr {Pattern = (Open :: _); Func = _}, _) :: rest -> findClosed rest
    | other :: rest -> Some other

// Note, don't move the text pointer when finishing an open expression, so that the parent expression is closed.
let (|FinishOpenExpression|_|) (typesStack: SubexpressionAndOffset list) (text: StringWindow) =
        match typesStack with
        | (RSubExpr current, offset) :: rest -> 
            match current.Pattern with
            | (Open) :: [] 
            | (RCap _) :: Open :: [] -> 
                match findClosed rest with
                | Some (RSubExpr ancestor, ancestorOffset) -> 
                    match ancestor.Pattern with
                    | ((SCap h) :: rest) 
                    | ((RCap h) :: rest) when text.StartsWith(h) -> Some (current, offset, text)
                    | _ -> None                    
                | None when text.Length = 0u -> Some (current, offset, text)
                | _ -> None
            | _ -> None
        | _ -> None

let parseProgram (startText: string) = 
    let rec parseProgramInner (str: StringWindow) (result: ExprRep list) (currentCaptures: SubexpressionAndOffset list) : (StringWindow * ExprRep) =
        try
            match result with
            | { Offset = cSubExprOffset; Expr = SubExpression cSubExpr } :: rSubExprs -> 
                let getCurrentSubexpression () = SubExpression (cSubExpr |> List.rev)
                match str with
                | FinishOpenExpression currentCaptures (subtype, expressionStartOffset, crem) ->
                    let length = (uint32 str.Offset) - expressionStartOffset
                    let innerResult = { Offset = expressionStartOffset; Length = length; Expr = getCurrentSubexpression () } :: rSubExprs  
                    let value = innerResult |> List.rev |> subtype.Func in 
                        crem, value
                | _ when str.Length = 0u -> 
                    // End of the road, wrap unclosed expressions in a subexpression
                    let cSubExprRevOffset = (cSubExpr |> List.rev).Head.Offset
                    let newSubExpr = { Offset = str.Offset; Length = str.Offset - cSubExprRevOffset; Expr = getCurrentSubexpression () } 
                    str, listToSubExpression (newSubExpr :: rSubExprs)            
                | OngoingExpression currentCaptures (captures, crem) ->
                    match captures with
                    // Resolved Expression is Finished (No more pattern to match)
                    | (RSubExpr { Pattern = []; Func = func }, offset) :: parents -> 
                        let subExprRep = { Offset = offset; Length = crem.Offset - cSubExprOffset; Expr = getCurrentSubexpression () }
                        let innerResult = subExprRep :: rSubExprs  
                        let value = innerResult |> List.rev |> func in 
                            crem, value   
                    // Expression Continues (more pattern to go)
                    | (_, prevOffset) :: parents -> 
                        let fresh = { Offset = str.Offset; Length = UInt32.MaxValue; Expr = SubExpression [] }
                        let stale = { Offset = prevOffset; Length = str.Offset - prevOffset; Expr = getCurrentSubexpression () }
                        parseProgramInner crem (fresh :: stale :: rSubExprs) captures
                    | [] -> failwith "Unexpected output from OngoingExpression"
                | RefineOpenExpression currentCaptures (subtype, crem) ->
                    // Mid-Expression we find evidence that further restricts the kind of expression it is
                    let fresh = { Offset = str.Offset; Length = UInt32.MaxValue; Expr = SubExpression [] }
                    // stale is the first finished chunk as defined by the first delimiter of the open expression
                    let stale = { Offset = cSubExprOffset; Length = crem.Offset - cSubExprOffset; Expr = getCurrentSubexpression () }
                    let rem, value = parseProgramInner crem (fresh :: stale :: []) ((RSubExpr subtype, str.Offset) :: currentCaptures) 
                    // Return when the refined expression has finished.
                    let finalExpr = {value with Expr = SubExpression [value]}
                    parseProgramInner rem (finalExpr :: rSubExprs) currentCaptures    
                | NewExpression currentCaptures (subtype, crem) ->
                    let newExpr = { Offset = str.Offset; Length = UInt32.MaxValue; Expr = SubExpression [] }   
                    let rem, value = parseProgramInner crem [newExpr] ((subtype, str.Offset) :: currentCaptures)  
                    // Return when the new expression has finished.
                    let finalExpr = { Offset = crem.Offset; Length = rem.Offset - str.Offset; Expr = SubExpression (value :: cSubExpr)}   
                    parseProgramInner rem (finalExpr :: rSubExprs) currentCaptures
                | Skip whitespaceVocabulary res
                | Num res
                | MapSymbol res
                | CaptureChar '`' res
                | CaptureString '"' res
                | CaptureString ''' res 
                | CaptureUnknown endUnknownChars res ->
                    let v, rem = res 
                    match v with
                    | Some value -> 
                        let newExpr = { Offset = str.Offset; Length = rem.Offset - str.Offset; Expr = value }
                        let newExprs = { Offset = cSubExprOffset; Length = rem.Offset - cSubExprOffset; Expr = SubExpression (newExpr :: cSubExpr) }
                        parseProgramInner rem (newExprs :: rSubExprs) currentCaptures
                    | None -> parseProgramInner rem result currentCaptures
                | str -> parseProgramInner (str.Subwindow(1u)) result currentCaptures
            | _ -> failwith "Expected a SubExpression"
        with | :? BarbParsingException as ex -> raise ex
             | ex -> raise <| new BarbParsingException(ex.Message, str.Offset, str.Length)
    let _, res = parseProgramInner (StringWindow(startText, 0u)) [{ Offset = 0u; Length = 0u; Expr = SubExpression [] }] [] in res