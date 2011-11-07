module Barb.Internals

open System
open System.Text
open System.Text.RegularExpressions
open System.Reflection
open System.ComponentModel
open System.Linq
open System.Collections.Concurrent
open System.Collections.Generic

open Microsoft.FSharp.Reflection

// Helpers

module Option =
    let tryResolve (func: unit -> _ option) (opt: _ option) =
        match opt with
        | Some value -> Some value
        | None -> func()

let memoizeBy inputToKey f =
    let cache = ConcurrentDictionary<_, _>()
    fun x ->
        let k = inputToKey x
        if cache.ContainsKey(k) then cache.[k]
        else 
            let res = f x
            cache.[k] <- res
            res


// Actual Implementation

type MethodSig = ((obj array -> obj) * Type array) list

type MemberTypes = 
    | PropertyCall of (obj -> obj)
    | IndexedPropertyCall of (obj -> ((obj array -> obj) * Type array) list)
    | MethodCall of (obj -> ((obj array -> obj) * Type array) list)

type ExprTypes = 
    | ParentProperty of (obj -> obj)
    | Method of MethodSig
    | IndexedProperty of MethodSig
    | BoolToBoolToBool of (bool -> bool -> bool)
    | BoolToBool of (bool -> bool)
    | Bool of bool
    | ObjToObjToBool of (obj -> obj -> bool)
    | ObjToBool of (obj -> bool)
    | ObjToObj of (obj -> obj)  
    | Obj of obj
    | Returned of obj
    | Infix of ExprTypes
    | Unit
    | SubExpression of ExprTypes list
    | Tuple of ExprTypes list
    | ResolvedTuple of ExprTypes list
    | IndexArgs of ExprTypes
    | ResolvedIndexArgs of ExprTypes
    | Invoke
    | AppliedInvoke of string
    | Unknown of string
    | Binding of string * ExprTypes

let nullableToOption res =
    match res with
    | null -> None
    | item -> Some item

let resolveMember (rtype: System.Type) (memberName: string) =
    let resolveProp () = 
        rtype.GetProperty(memberName) |> nullableToOption
        |> function
           | None -> None
           | Some prop -> match prop.GetIndexParameters() with
                          | [||] -> Some <| PropertyCall (fun obj -> prop.GetValue(obj, null))
                          | prms ->
                                let typeArgs = prms |> Array.map (fun pi -> pi.ParameterType)
                                Some <| IndexedPropertyCall (fun obj -> [(fun args -> prop.GetValue(obj, args)), typeArgs])
    let resolveMethod () = 
       rtype.GetMethods()
       |> Array.filter (fun mi -> mi.Name = memberName) |> Array.toList
       |> function
          | [] -> None
          | list -> 
            let methodsWithParams = 
                list |> List.map (fun mi -> mi, mi.GetParameters() |> Array.map (fun pi -> pi.ParameterType))
            let globallyResolvedMethods instance =
                methodsWithParams
                |> List.map (fun (mi, mp) ->
                                let callMethod = 
                                    fun args ->
                                        if instance <> null then mi.Invoke(instance, args)
                                        else null
                                callMethod, mp)
            Some <| MethodCall (globallyResolvedMethods)
    let resolveField () = 
        rtype.GetField(memberName) |> nullableToOption
        |> function
            | Some fld -> Some <| PropertyCall (fun obj -> fld.GetValue(obj, null))
            | None -> None  
    resolveProp () |> Option.tryResolve resolveMethod |> Option.tryResolve resolveField

let rec resolveAllProperties (rtype: System.Type) (parentName: string) (getter: obj -> obj) =
    let properties = rtype.GetProperties()
    let methodCollections = rtype.GetMethods()
                            |> Seq.groupBy (fun mi -> mi.Name)                                        
    seq {
        for prop in properties do
            let fullName =
                if String.IsNullOrEmpty parentName then prop.Name 
                else String.Format("{0}.{1}", parentName, prop.Name)
            let getPropFunc = 
                fun (instance:obj) -> 
                    let parentResult = getter instance
                    prop.GetValue(parentResult, null)
            yield fullName, ParentProperty getPropFunc
    }     


let convertToTargetType (ttype: Type) (param: obj) = 
    if param = null then Some null
    else
        let des = TypeDescriptor.GetConverter(ttype)
        match des.CanConvertFrom(param.GetType()) with
        | true -> Some <| des.ConvertFrom(param)
        | false -> None            

let (|DecomposeOption|_|) (o: obj) = 
    if o = null then Some null
    else
        let genericOptionType = typedefof<_ option>
        let bindingFlags = BindingFlags.GetProperty ||| BindingFlags.Instance ||| BindingFlags.Public      
        match o.GetType() with
        | t when t.IsGenericType -> 
            let gt = t.GetGenericTypeDefinition()
            if gt = genericOptionType then
                Some <| t.InvokeMember("Value", bindingFlags, null, o, Array.empty)
            else None
        | _ -> None        

let resolveResultType = 
    fun (output: obj) ->
        match output with
        | :? bool as boolinstance -> Bool boolinstance
        | DecomposeOption contents -> Obj contents
        | other -> Obj other

let cachedResolveMember = 
    let inputToKey (rtype: System.Type, caseName) = rtype.FullName + "~" + caseName
    let resolveMember (rtype, caseName) = resolveMember rtype caseName
    memoizeBy inputToKey resolveMember

let resolveInvoke (o: obj) (memberName: string) =
    if o = null then None
    else cachedResolveMember (o.GetType(), memberName)
         |> Option.map (function
                        | PropertyCall pc -> Returned (pc o)
                        | MethodCall mc -> Method (mc o)
                        | IndexedPropertyCall ipc -> IndexedProperty (ipc o))

let executeUnitMethod (sigs: MethodSig) =
    sigs 
    |> List.tryFind (fun (exec, paramTypes) -> paramTypes.Length = 0)
    |> Option.bind (fun (exec, paramTypes) -> Some <| exec Array.empty)
    |> Option.map Returned

let executeOneParamMethod (sigs: MethodSig) (param: obj) =
    sigs
    |> List.tryFind (fun (exec, paramTypes) -> paramTypes.Length = 1)
    |> Option.bind (fun (exec, paramTypes) -> 
        convertToTargetType (paramTypes.[0]) param 
        |> Option.map (fun converted -> exec, converted))
    |> Option.map (fun (exec, converted) -> exec [| converted |])
    |> Option.map Returned

let executeParameterizedMethod (sigs: MethodSig) (prms: ExprTypes list) =
    let arrayPrms = 
        prms 
        |> List.map (function | Obj o -> o | ohno -> failwith (sprintf "Unexpected parameter type: %A" ohno))
        |> List.toArray
    sigs
    |> List.tryFind (fun (exec, paramTypes) -> paramTypes.Length = arrayPrms.Length)
    |> Option.map (fun (exec, paramTypes) -> 
        Array.zip paramTypes arrayPrms
        |> Array.map (fun (tType, param) -> convertToTargetType tType param)
        |> Array.map (function | Some rParam -> rParam | None -> failwith (sprintf "Unable to resolve method parameters: %A -> %A" arrayPrms paramTypes))
        |> fun rParams -> exec rParams)
    |> Option.map Returned

let resolveObjectIndexer (rtype: System.Type) =
    let indexers = rtype.GetCustomAttributes(typeof<DefaultMemberAttribute>, true) |> Array.map (fun t -> t :?> DefaultMemberAttribute)
    match indexers with
    | [| |] -> None 
    | attrs -> let memberName = attrs.[0].MemberName    
               let pi = rtype.GetProperty(memberName) in Some (pi, pi.GetIndexParameters())

let cachedResolveObjectIndexer = 
    let inputToKey (rtype: System.Type) = rtype.FullName
    let resolveValue rtype = resolveObjectIndexer rtype
    memoizeBy inputToKey resolveValue

let callIndexedProperty (target: obj) (indexVal: obj) =
    if target = null then Some <| Obj null
    else
        let ttype = target.GetType()
        match cachedResolveObjectIndexer ttype with
        | None -> None
        | Some (propInfo, paramInfos) -> 
            match paramInfos with
            | [| |] -> failwith (sprintf "Expected an indexed object, but got non-indexed: %s" ttype.FullName)
            | [| arg |] -> 
                match convertToTargetType (arg.ParameterType) indexVal with
                | Some (converted) -> Some <| (Returned (propInfo.GetValue(target, [| converted |])))
                | None -> failwith (sprintf "No conversion found from %s to %s" (string indexVal) ttype.FullName)                
            | other -> failwith (sprintf "MultiIndexed objects are not currently supported: %s" ttype.FullName)

let rec applyInstanceState (input: obj) exprs =
    let rec resolveInstanceType expr =
            match expr with 
            | ParentProperty (call) -> Returned (call input)
            | SubExpression (subEx) -> SubExpression (applyInstanceState input subEx)
            | Tuple (tuple) -> Tuple (applyInstanceState input tuple) 
            | IndexArgs (argEx) -> IndexArgs (resolveInstanceType argEx)
            | other -> other
    exprs |> List.map (fun expr -> resolveInstanceType expr)
    
let resolveExpression exprs (failOnUnresolved: bool) = 
    let rec (|ResolveSingle|_|) =
        function 
        | Returned o -> Some <| resolveResultType o
        | Tuple tc -> 
            let resolvedTp = tc |> List.collect (fun t -> reduceExpressions [] [t]) |> List.rev |> ResolvedTuple
            Some resolvedTp
        | Unknown unk -> Some <| Obj unk
        | _ -> None

    and attemptToResolvePair =
        function
        | ObjToObj l, Obj r -> Some <| Obj (l r)
        | Obj l, (Infix (ObjToObjToBool r)) -> Some <| ObjToBool (r l)
        | Bool l, (Infix (ObjToObjToBool r)) -> Some <| ObjToBool (r l)
        | Bool l, (Infix (BoolToBoolToBool r)) -> Some <| BoolToBool (r l)
        | ObjToBool l, Obj r -> Some <| Bool (l r)
        | ObjToBool l, Bool r -> Some <| Bool (l r)
        | BoolToBool l, Bool r -> Some <| Bool (l r)
        | Method l, Unit -> executeUnitMethod l
        | Method l, Obj r -> executeOneParamMethod l r
        | Method l, ResolvedTuple r -> executeParameterizedMethod l r 
        | Invoke, Unknown r -> Some <| AppliedInvoke r
        | Invoke, ResolvedIndexArgs r -> Some <| ResolvedIndexArgs r //Here for F#-like indexing (if you want it)
        | Obj l, AppliedInvoke r -> resolveInvoke l r
        | IndexedProperty l, ResolvedIndexArgs (Obj r) -> executeOneParamMethod l r
        | Obj l, ResolvedIndexArgs (Obj r) -> callIndexedProperty l r
        | _ -> None

    and reduceExpressions lleft lright =
        match lleft, lright with
        | (ResolveSingle resolved :: lt), right -> reduceExpressions lt (resolved :: right)
        | left, (SubExpression exp :: rt) ->
            match reduceExpressions [] exp |> List.rev with
            | single :: [] -> reduceExpressions left (single :: rt)
            | many -> reduceExpressions (SubExpression many :: left) rt
        | left, (IndexArgs exp :: rt) ->
            match reduceExpressions [] [exp] with
            | [] -> failwith (sprintf "No indexer found in [ ]")
            | single :: [] -> reduceExpressions left (ResolvedIndexArgs single :: rt)
            | other -> failwith (sprintf "Multi-indexing not currently supported")
        | [], r :: rt -> reduceExpressions [r] rt
        | l :: [], [] -> [l]
        | l :: lt, r :: rt ->        
            match attemptToResolvePair (l, r) with
            | Some (rToken) -> reduceExpressions lt (rToken :: rt)
            | None -> reduceExpressions (r :: l :: lt) rt
        | catchall when failOnUnresolved -> failwith (sprintf "Unexpected case: %A" catchall)
        | left, [] -> left

    reduceExpressions [] exprs

let compareAsSameType obj1 obj2 func =
    let converted = 
        if obj1 <> null && obj2 <> null && obj1.GetType() = obj2.GetType() then obj2
        else
            let t1Des = TypeDescriptor.GetConverter(obj1.GetType())
            t1Des.ConvertFrom(obj2)
    func obj1 converted
            
let objectsEqual (obj1: obj) (obj2: obj) = 
    if obj1 = null && obj2 = null then true
    elif obj1 = null || obj2 = null then false
    else compareAsSameType obj1 obj2 (fun o1 o2 -> o1.Equals(o2))

let objectsNotEqual = 
    (fun o1 o2 -> not (objectsEqual o1 o2))

let compareObjects op =
    fun (obj1: obj) (obj2: obj) ->
        match obj1, obj2 with
        | null, null -> false
        | null, _ | _, null -> false
        | (:? IComparable as comp1), (:? IComparable as comp2) when obj1.GetType() = obj2.GetType() -> op comp1 comp2
        | (:? IComparable as comp1), obj2 -> compareAsSameType comp1 obj2 (fun o1 o2 -> op o1 (o2 :?> IComparable))
        | _ -> failwith (sprintf "Unable to compare %A and %A" obj1 obj2)

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

let (|Capture|_|) (bStr: string) (eStr: string) (text: string) =
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

let (|DelimitedCapture|_|) (bStr: string) (delimStr: string) (eStr: string) (text: string) =
    match text with
    | Capture bStr eStr (contents, remainder) -> 
        let contentEnd = contents.Length
        let _, substrings = 
            allIndicesOf delimStr text 
            |> (fun seps -> seps @ [contentEnd + 1])
            |> List.fold (fun (last, texts) next -> next, contents.Substring(last, next - last - 1) :: texts) (0, [])
        Some (substrings |> List.rev, remainder)
    | _ -> None  

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
    let rec parseProgramInner (str: string) (parsed: ExprTypes list) =
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



let buildExpression (localType: Type) (predicate: string) : (obj -> obj) =

    let memberMap =
        let keyValues = 
            resolveAllProperties localType "" id
            |> Seq.map (fun (k,v) -> new KeyValuePair<_,_>(k,v))
        new ConcurrentDictionary<_, _>(keyValues)

    let getMember memberName =
        match memberMap.TryGetValue memberName with
        | true, foundMember -> Some foundMember
        | false, _ -> None

    let tokens = tokenizeString predicate

#if DEBUG
    printfn "T: %A" tokens
#endif

    let parsedTokens = 
        parseTokens getMember tokens
        |> (fun (res, remainder, expType) -> res)
//    let parsedTokens = parseProgram getMember predicate

#if DEBUG
    printfn "PT: %A" parsedTokens
#endif

    let reducedExpression = 
        resolveExpression parsedTokens false |> List.rev

#if DEBUG
    printfn "RE: %A" reducedExpression
#endif

    let calculateResult input = 

        let appliedParsedTokens = applyInstanceState input reducedExpression
#if DEBUG        
        printfn "APT: %A" appliedParsedTokens
#endif
        match resolveExpression appliedParsedTokens true with
        | Obj (res) :: [] -> res
        | Bool (res) :: [] -> box res
        | otherToken -> failwith (sprintf "Unexpected result: %A" otherToken)

    calculateResult


