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
    | MethodCall of (obj -> ((obj array -> obj) * Type array) list)

type ExprTypes = 
    | ParentProperty of (obj -> obj)
    | Method of MethodSig
    | ObjToObjToBool of (obj -> obj -> bool)
    | BoolToBoolToBool of (bool -> bool -> bool) 
    | ObjToObj of (obj -> obj)  
    | ObjToBool of (obj -> bool)
    | BoolToBool of (bool -> bool)
    | Bool of bool
    | Obj of obj
    | Returned of obj
    | Infix of ExprTypes
    | Unit
    | SubExpression of ExprTypes list
    | Tuple of ExprTypes list
    | ResolvedTuple of ExprTypes list
    | Invoke
    | AppliedInvoke of string
    | Unknown of string

let nullableToOption res =
    match res with
    | null -> None
    | item -> Some item

let resolveMember (rtype: System.Type) (memberName: string) =
    let resolveProp () = 
        rtype.GetProperty(memberName) |> nullableToOption
        |> function
           | Some prop -> Some <| PropertyCall (fun obj -> prop.GetValue(obj, null))
           | None -> None
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
                        | MethodCall mc -> Method (mc o))

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

let rec applyInstanceState (input: obj) exprs =
    let rec resolveInstanceType expr =
            match expr with 
            | ParentProperty (call) -> Returned (call input)
            | SubExpression (subEx) -> SubExpression (applyInstanceState input subEx)
            | Tuple (tuple) -> Tuple (applyInstanceState input tuple) 
            | other -> other
    exprs |> List.map (fun expr -> resolveInstanceType expr)
    
let resolveExpression exprs = 
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
        | Bool l, (Infix (BoolToBoolToBool r)) -> Some <| BoolToBool (r l)
        | ObjToBool l, Obj r -> Some <| Bool (l r)
        | BoolToBool l, Bool r -> Some <| Bool (l r)
        | Method l, Unit -> executeUnitMethod l
        | Method l, Obj r -> executeOneParamMethod l r
        | Method l, ResolvedTuple r -> executeParameterizedMethod l r 
        | Invoke, Unknown r -> Some <| AppliedInvoke r
        | Obj r, AppliedInvoke l -> resolveInvoke r l
        | _ -> None

    and reduceExpressions left right =
        match left, right with
        | [], r :: rt -> reduceExpressions [r] rt
        | (ResolveSingle resolved :: lt), right -> reduceExpressions lt (resolved :: right)
        | (SubExpression exp :: lt), right ->
            let resolvedSub = reduceExpressions [] exp
            reduceExpressions lt (resolvedSub @ right)
        | l :: [], [] -> [l]
        | l :: lt, r :: rt ->        
            match attemptToResolvePair (l, r) with
            | Some (rToken) -> reduceExpressions lt (rToken :: rt)
            | None -> reduceExpressions (r :: l :: lt) rt
        | catchall -> failwith (sprintf "Unexpected case: %A" catchall)

    reduceExpressions [] exprs

let compareAsSameType obj1 obj2 func =
    let t1Des = TypeDescriptor.GetConverter(obj1.GetType())
    func obj1 (t1Des.ConvertFrom(obj2))
            
let objectsEqual (obj1: obj) (obj2: obj) = 
    if obj1 = null && obj2 = null then true
    elif obj1 = null || obj2 = null then false
    else compareAsSameType obj1 obj2 (fun o1 o2 -> o1.Equals(o2))

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

let tokenizeString (str: string) = 
    let whitespace = [| yield ' '; yield! Environment.NewLine |]
    let tokenChars = [| ','; ')'; '('; '.' |]
    let rec findTokens currentIndex beginIndex quoteMode pairs = 
        match currentIndex, beginIndex with
        | -1, -1 -> pairs
        | -1, bi when quoteMode = false -> (0, bi, quoteMode) :: pairs
        // Parse out chars which always count as their own tokens (unless in quotes)
        | ci, bi when tokenChars.Contains str.[ci] && quoteMode = false -> 
            let paramToken = ci, ci, quoteMode
            if bi = -1 then findTokens (ci - 1) -1 false (paramToken :: pairs)
            else findTokens (ci - 1) -1 false (paramToken :: (ci + 1, beginIndex, quoteMode) :: pairs)
        // Close Quote
        | ci, -1 when str.[ci] = '\"' -> findTokens (ci - 1) (ci - 1) true pairs
        // Open Quote
        | ci, bi when str.[ci] = '\"' && quoteMode = true -> findTokens (ci - 1) -1 false ((ci + 1, beginIndex, quoteMode) :: pairs)
        // Non-Quoted Token Start
        | ci, -1 when not <| whitespace.Contains str.[ci] -> findTokens (ci - 1) ci false pairs
        // Non-Quoted Token End
        | ci, bi when whitespace.Contains str.[ci] && bi <> -1 && not quoteMode -> findTokens (ci - 1) -1 false ((ci + 1, beginIndex, quoteMode) :: pairs)
        // Continue
        | ci, bi -> findTokens (ci - 1) bi quoteMode pairs
    findTokens (str.Length - 1) -1 false [] 
    |> List.map (fun (b, e, quoted) -> let resString = str.Substring(b, e - b + 1) 
                                       let tokenType = if quoted then Quoted else Normal
                                       resString, tokenType)

type TokenPair = string * StringTokenType
 
let inline (|ResolveConstants|_|) token =
    match token with
    | "null" -> Some <| Obj null
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

let inline (|BooleanOperation|_|) token = 
    match token with
    | "&" | "&&" | "and" -> Some <| Infix (BoolToBoolToBool (&&))
    | "|" | "||" | "or"  -> Some <| Infix (BoolToBoolToBool (||))
    | _ -> None

let inline (|GetOperation|_|) getMember (token: string) = 
    getMember token

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
                | EndSubExpression -> 
                    match expressionType with
                    | StandardExpression -> collectedResult |> List.rev, rest, expressionType
                    | TupleExpression -> 
                        let revCollected = collectedResult |> List.rev
                        (SubExpression revCollected :: result), rest, expressionType
                | TupleIndicator ->
                    let reversedResult = collectedResult |> List.rev 
                    parseTokensInner rest [] (SubExpression reversedResult :: result) TupleExpression
                | CallIndicator op
                | ResolveConstants op
                | BooleanOperation op
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

    //printfn "T: %A" tokens

    let parsedTokens = 
        parseTokens getMember tokens
        |> (fun (res, remainder, expType) -> res)

    //printfn "PT: %A" parsedTokens

    let calculateResult input = 

        let appliedParsedTokens = applyInstanceState input parsedTokens
        
        //printfn "APT: %A" appliedParsedTokens
        
        match resolveExpression appliedParsedTokens with
        | Obj (res) :: [] -> res
        | Bool (res) :: [] -> box res
        | otherToken -> failwith (sprintf "Unexpected result: %A" otherToken)

    calculateResult


