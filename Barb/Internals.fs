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

type UnresolvedInstanceTypes = 
    | PropertyCall of (obj -> obj)    
    | MethodCall of (obj -> ((obj array -> obj) * Type array) list)
    | DynamicCall of string

type MethodSig = ((obj array -> obj) * Type array) list

type ExprTypes = 
    | ObjToObjToBool of (obj -> obj -> bool)
    | BoolToBoolToBool of (bool -> bool -> bool) 
    | ObjToObj of (obj -> obj)  
    | ObjToBool of (obj -> bool)
    | BoolToBool of (bool -> bool)
    | Bool of bool
    | Obj of obj
    | Returned of obj
    | Infix of ExprTypes
    | UnresolvedInstanceType of UnresolvedInstanceTypes
    | Method of MethodSig
    | Unit
    | SubExpression of ExprTypes list
    | TupleSeparator
    | Tuple of ExprTypes list
    | ResolvedTuple of ExprTypes list    

let nullableToOption res =
    match res with
    | null -> None
    | item -> Some item

let rec resolveMembers (rtype: System.Type) (parentName: string) (getter: obj -> obj) =
    let properties = rtype.GetProperties()
    let methodCollections = rtype.GetMethods()
                            |> Seq.groupBy (fun mi -> mi.Name)                                        
    seq {
        for methodName, methodInfos in methodCollections do
            let fullName =
                if String.IsNullOrEmpty parentName then methodName 
                else String.Format("{0}.{1}", parentName, methodName)
            let methodInstances = 
                methodInfos 
                |> Seq.toList
                |> List.map (fun mi -> 
                                let methodParams = 
                                    mi.GetParameters()
                                    |> Array.map (fun pi -> pi.ParameterType)
                                mi, methodParams)
            let globallyResolvedMethods instance =
                methodInstances
                |> List.map (fun (mi, mp) ->
                                let callMethod = 
                                    fun args ->
                                        let methodParent = getter instance
                                        if methodParent <> null then mi.Invoke(methodParent, args)
                                        else null
                                callMethod, mp)
            yield fullName, MethodCall (globallyResolvedMethods)

        for prop in properties do
            let fullName =
                if String.IsNullOrEmpty parentName then prop.Name 
                else String.Format("{0}.{1}", parentName, prop.Name)
            let getPropFunc = 
                fun (instance:obj) -> 
                    let parentResult = getter instance
                    prop.GetValue(parentResult, null)
            yield fullName, PropertyCall getPropFunc
    }     

// This is awful and needs to be rewritten I know, just a temp fix for now
let internal getCachedDelayedMembers (baseMemberCache: ConcurrentDictionary<_,_>) =
    let cachedChildren = new HashSet<_>() 
    let memberCache = new ConcurrentDictionary<_, _>(baseMemberCache)

    let findLowestCommonMember (memberName: string) =
        seq {
            for i = memberName.Length - 1 downto 0 do
                if memberName.[i] = '.' then yield i
        }
        |> Seq.tryFind (fun i -> memberName.Substring(0, i) |> memberCache.ContainsKey)
        |> Option.map (fun i -> 
                let parentName = memberName.Substring(0, i)
                let childName = memberName.Substring(i + 1, memberName.Length - 1 - i)
                parentName, childName, memberCache.[parentName])
    
    let rec updateCacheWithNewType memberName instance =
        let parent, child, parentMember = 
            match findLowestCommonMember memberName with
            | Some (result) -> result            
            | None -> failwith (sprintf "Base member not found: %s" memberName)          

        if not <| cachedChildren.Contains(parent) then 
            let parentProp = 
                parentMember 
                |> function 
                   | PropertyCall (call) -> call 
                   | other -> failwith (sprintf "Expected a property, but got a member while doing a dynamic lookup for: %s" memberName)

            let childInstance = parentProp instance

            resolveMembers (childInstance.GetType()) parent parentProp
            |> Seq.iter (fun (k,v) -> memberCache.TryAdd(k,v) |> ignore)
            cachedChildren.Add(parent) |> ignore
            
            getFromCache memberName instance
        else
            failwith (sprintf "Delayed lookup failed on parent (%s) and child (%s), member not found in type" parent child)

    and getFromCache memberName instance =
        match memberCache.TryGetValue memberName with
        | true, foundMember -> foundMember
        | false, _ -> updateCacheWithNewType memberName instance

    getFromCache

                   
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

let rec (|ResolveSingle|_|) =
    function 
    | Returned o -> Some <| resolveResultType o
    | Tuple tc -> 
        let resolvedTp = tc |> List.collect (fun t -> resolve [] [t]) |> List.rev |> ResolvedTuple
        Some resolvedTp
    | _ -> None

and attemptToResolvePair =
    function
    | ObjToObj l, Obj r -> Some <| Obj (l r)
    | Obj l, (Infix (ObjToObjToBool r)) -> Some <| ObjToBool (r l)
    | (Infix (ObjToObjToBool l)), Obj r -> Some <| ObjToBool (fun e -> l e r)
    | Bool l, (Infix (BoolToBoolToBool r)) -> Some <| BoolToBool (r l)
    | (Infix (BoolToBoolToBool l)), Bool r -> Some <| BoolToBool (fun e -> l e r)
    | ObjToBool l, Obj r -> Some <| Bool (l r)
    | BoolToBool l, Bool r -> Some <| Bool (l r)
    | Method l, Unit -> executeUnitMethod l
    | Method l, Obj r -> executeOneParamMethod l r
    | Method l, ResolvedTuple r -> executeParameterizedMethod l r 
    | _ -> None

and resolve left right =
    match left, right with
    | [], r :: rt -> resolve [r] rt
    | (ResolveSingle resolved :: lt), right -> resolve lt (resolved :: right)
    | (SubExpression exp :: lt), right ->
        let resolvedSub = resolve [] exp
        resolve lt (resolvedSub @ right)
    | l :: [], [] -> [l]
    | l :: lt, r :: rt ->        
        match attemptToResolvePair (l, r) with
        | Some (rToken) -> resolve lt (rToken :: rt)
        | None -> resolve (r :: l :: lt) rt
    | catchall -> failwith (sprintf "Unexpected case: %A" catchall)

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
    let tokenChars = [| ','; ')'; '(' |]
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
    | "=" -> Some <| Infix (ObjToObjToBool objectsEqual)
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
    match getMember token with
    | Some (getter) -> Some <| (UnresolvedInstanceType getter)
    | None -> if token.Contains(".") 
                then Some <| (UnresolvedInstanceType <| DynamicCall token)
                else None

let inline (|BeginSubExpression|_|) token =
    match token with
    | "(" -> Some() | _ -> None 
  
let inline (|EndSubExpression|_|) token =
    match token with
    | ")" -> Some() | _ -> None

let inline (|TupleIndicator|_|) token =
    match token with
    | "," -> Some TupleSeparator | _ -> None

let rec resolveTuples subExpr collected resolved inTuple =
    match subExpr, collected with
    | [], [] -> resolved, inTuple
    | [], collected when inTuple -> resolveTuples [] [] (SubExpression collected :: resolved) inTuple
    | [], cH :: cT when not inTuple -> resolveTuples [] cT (cH :: resolved) inTuple
    | TupleSeparator :: rest, collected -> resolveTuples rest [] (SubExpression collected :: resolved) true
    | SubExpression (exp) :: rest, collected -> let subResults, wasTuple = resolveTuples exp [] [] false 
                                                let maybeTuple =
                                                    let revSubResults = subResults
                                                    if wasTuple then Tuple revSubResults else SubExpression revSubResults
                                                resolveTuples rest (maybeTuple :: collected) resolved inTuple
    | exprH :: exprT, collected -> resolveTuples exprT (exprH :: collected) resolved inTuple
    | _ -> failwith (sprintf "Failed resolving tuples on %A -- %A" subExpr collected)

let parseTokens getMember tokens = 
    let rec parseTokensInner tokens (result: ExprTypes list) = 
        match tokens with
        | [] -> result |> List.rev, []
        | head :: rest -> 
            let contents, tokenType = head
            match tokenType with
            | Quoted -> parseTokensInner rest (Obj contents :: result)
            | Normal -> 
                match contents with
                | BeginSubExpression -> 
                    let exprs, unused = parseTokensInner rest [] 
                    match exprs with
                    | [] -> parseTokensInner unused (Unit :: result)
                    | contents -> parseTokensInner unused (SubExpression exprs :: result)
                | EndSubExpression -> result |> List.rev, rest
                | ResolveConstants op
                | TupleIndicator op
                | BooleanOperation op
                | NotOperation op
                | CompareOperation op 
                | GetOperation getMember op -> parseTokensInner rest (op :: result)
                | unrecognized -> parseTokensInner rest (Obj unrecognized :: result)
                | _ -> failwith (sprintf "Unexpected Token: %A" head)
    parseTokensInner tokens []

let buildExpression (localType: Type) (predicate: string) : (obj -> obj) =

    let memberMap =
        let keyValues = 
            resolveMembers localType "" id
            |> Seq.map (fun (k,v) -> new KeyValuePair<_,_>(k,v))
        new ConcurrentDictionary<_, _>(keyValues)

    let getMember memberName =
        match memberMap.TryGetValue memberName with
        | true, foundMember -> Some foundMember
        | false, _ -> None

    let getDelayedMember = getCachedDelayedMembers memberMap

    let tokens = tokenizeString predicate

    let parsedTokens = 
        parseTokens getMember tokens
        |> (fun (res, remainder) -> resolveTuples res [] [] false)
        |> (fun (res, wasTuple) -> res)

    let calculateResult input = 
        let appliedParsedTokens = 
            let rec resolveInstanceType instanceType =
                match instanceType with
                | PropertyCall (call) -> Returned (call input)
                | MethodCall (call) -> let resolved = call input in Method resolved
                | DynamicCall (name) -> resolveInstanceType (getDelayedMember name input)

            let rec resolveParams exprs = 
                exprs 
                |> List.map (function 
                             | UnresolvedInstanceType (itype) -> resolveInstanceType itype
                             | SubExpression (subEx) -> SubExpression (resolveParams subEx)
                             | Tuple (tuple) -> Tuple (resolveParams tuple)
                             | other -> other)
            resolveParams parsedTokens
        
        match resolve [] appliedParsedTokens with
        | Obj (res) :: [] -> res
        | Bool (res) :: [] -> box res
        | otherToken -> failwith (sprintf "Unexpected result: %A" otherToken)

    calculateResult


