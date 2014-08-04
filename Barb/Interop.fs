module Barb.Interop

open System
open System.Text
open System.Text.RegularExpressions
open System.Reflection
open System.Linq.Expressions
open System.ComponentModel
open System.Linq
open System.Linq.Expressions
open System.Collections
open System.Collections.Concurrent
open System.Collections.Generic
open System.Runtime.CompilerServices
open System.Numerics  

open Microsoft.FSharp.Reflection

open Barb.Helpers
open Barb.Representation

module OperatorFactory = 
    // Left is higher precedence
    // TODO/IDEA: Use ranges of values ot determine overlap and generate correct unifications for all pairs
    let private orderOfNumericConversions = 
        [| TypeCode.Double; TypeCode.Single;
           TypeCode.Int64; TypeCode.Int32; TypeCode.Int16; TypeCode.SByte;
           TypeCode.UInt64; TypeCode.UInt32; TypeCode.UInt16; TypeCode.Char; TypeCode.Byte |]

    let private expConv prm typ = Expression.Convert(prm, typ)
    type private BinaryExprFunc = Expression -> Expression -> BinaryExpression
    type private BinaryExprSpecialCases = (Type * Type * (obj -> obj -> obj)) []
    let private genDynamicOperatorFunc (binExpr: BinaryExprFunc) (specialCases:BinaryExprSpecialCases) (typ1: Type) (typ2: Type) : obj -> obj -> obj =
        match specialCases |> Array.tryFind (fun (sct1, sct2, scfun) -> sct1 = typ1 && sct2 = typ2) with
        | Some (_,_,func) -> func
        | None -> 
            let inline isPrimitiveNumeric (tc: TypeCode) = orderOfNumericConversions |> Array.exists ((=) tc)
            let tc1, tc2 = Type.GetTypeCode(typ1), Type.GetTypeCode(typ2)    
            let prms = [|Expression.Parameter(typeof<obj>); Expression.Parameter(typeof<obj>)|]
            let inline wrappedExpr p1 p2 = Expression.Convert(binExpr p1 p2, typeof<obj>)
            let typed1, typed2 = Expression.Convert(prms.[0], typ1), Expression.Convert(prms.[1], typ2)
            let contents = 
                // Case: Both the Primitive Number Types
                if isPrimitiveNumeric tc1 && isPrimitiveNumeric tc2 then
                    // Same Type Code
                    if tc1 = tc2 then wrappedExpr typed1 typed2
                    // Convert to Right to Left Type Code
                    elif orderOfNumericConversions |> Array.find (fun e -> e = tc1 || e = tc2) = tc1 then
                            wrappedExpr typed1 (Expression.Convert(typed2, typ1))
                    // Covert to Left to Right Type Code
                    else wrappedExpr (Expression.Convert(typed1, typ2)) typed2
                else wrappedExpr typed1 typed2 // Case: Last Ditch, Try Adding
            let lambda = Expression.Lambda<Func<obj, obj, obj>>(contents, prms)
            let func : Func<obj,obj,obj> = lambda.Compile()
            fun (o1: obj) (o2: obj) -> func.Invoke(o1, o2)

    let generateCachedNumericOperator opExpr specialCases : (unit -> (obj -> obj -> obj)) = 
        let innerFunc () = 
            let genFunc = genDynamicOperatorFunc opExpr specialCases
            let cache: (obj -> obj -> obj) Option ref = ref None
            fun (obj1: obj) (obj2: obj) -> 
                if obj1 = null || obj2 = null then null
                else
                    let result = try !cache |> Option.bind (fun func -> func obj1 obj2 |> Some) with ex -> None
                    match result with
                    | Some(res) -> res
                    | None -> let newFunc = genFunc (obj1.GetType()) (obj2.GetType()) 
                              do cache := newFunc |> Some
                              newFunc obj1 obj2 
        innerFunc

open OperatorFactory

let nullableToOption res =
    match res with
    | null -> None
    | item -> Some item

let (|DecomposeOption|_|) (o: obj) = 
    if o = null then Some null
    else
        let genericOptionType = typedefof<_ option>
        let bindingFlags = BindingFlags.GetProperty ||| BindingFlags.Instance ||| BindingFlags.Public      
        match o.GetType() with
        | t when t.IsGenericType -> 
            let gt = t.GetGenericTypeDefinition()
            if gt = genericOptionType then t.InvokeMember("Value", bindingFlags, null, o, Array.empty) |> Some
            else None
        | _ -> None  

let (|SupportedNumberType|_|) (input: obj) =
    match input with    
    | (:? byte as num) -> Some (int64 (int num) :> obj)
    | (:? sbyte as num) -> Some (int64 (int num) :> obj)
    | (:? int16 as num) -> Some (int64 (int32 num) :> obj)
    | (:? uint16 as num) -> Some (int64 (uint32 num) :> obj)
    | (:? int32 as num) -> Some (int64 num :> obj)
    | (:? uint32 as num) -> Some (int64 num :> obj)
//    | (:? int64 as num) -> Some (int64 num :> obj)
    | (:? uint64 as num) -> Some (int64 num :> obj)
    | (:? nativeint as num) -> Some (int64 (int64 num) :> obj)
    | (:? unativeint as num) -> Some (int64 (uint64 num) :> obj)
    | (:? decimal as num) -> Some (float num :> obj)
    | (:? single as num) -> Some (float num :> obj)
    | value -> None

let (|ConvertSequence|_|) (o: obj) =
    match o with
    | (:? IEnumerable as enum) -> 
        seq { 
            for e in enum do
                match e with
                | SupportedNumberType num -> yield num
                | other -> yield other
        } |> Seq.toArray |> Some
    | _ -> None

let convertPotentiallyTupled  (args: obj) =
    match args with
    | :? (obj array) as tuple -> tuple
    | _ as o -> [| o |]

let rec resolveResultType (output: obj) = 
    match output with
    | null -> Obj null
    | :? string -> Obj output
    | DecomposeOption contents -> resolveResultType contents
    | SupportedNumberType contents -> Obj contents
    | other -> Obj other

let fieldToExpr (fld: FieldInfo list) = fld |> FieldGet
//    fun (obj: obj) ->
//        fld.GetValue(obj, null) |> Returned

//let staticPropertyToExpr (rtype: System.Type) (prop: PropertyInfo) = 
//    match prop.GetIndexParameters() with
//    | [||] ->
//       let propexp = Expression.Property(null, prop)
//        let lambda = Expression.Lambda(propexp)
//        let compiledLambda = lambda.Compile()
//        (fun obj -> compiledLambda.DynamicInvoke() |> Returned)
//    | prms -> 
//        let typeArgs = prms |> Array.map (fun pi -> pi.ParameterType)
//        (fun (obj: obj) -> [(fun args -> prop.GetValue(obj, args)), typeArgs] |> IndexedProperty)

let validateProperty (prop: PropertyInfo)  =
    match prop.GetIndexParameters() with
    | [||] when prop.CanRead -> prop
    | [||] when not <| prop.CanRead -> failwithf "Accessed property is not readable: %s" prop.Name
    | _ -> failwithf "Indexed Properties are not currently supported: %s" prop.Name

let tryGetProperty (getStatic: bool) (rtype: System.Type) (name: string) : PropertyInfo list = 
    let bindingFlags = if getStatic then BindingFlags.Static ||| BindingFlags.Public else BindingFlags.Instance ||| BindingFlags.Public
    let propInfos = rtype.GetProperties(bindingFlags) |> Array.filter (fun pi -> pi.Name = name) |> Array.filter (fun pi -> pi.GetIndexParameters().Length = 0)
    match propInfos with
    | [||] -> []
    | ps -> ps |> Array.map (validateProperty) |> Array.toList    

let tryGetIndexedProperty (getStatic: bool) (rtype: System.Type) (name: string) : PropertyInfo list = 
    let bindingFlags = if getStatic then BindingFlags.Static ||| BindingFlags.Public else BindingFlags.Instance ||| BindingFlags.Public
    let propInfos = rtype.GetProperties(bindingFlags) |> Array.filter (fun pi -> pi.Name = name) |> Array.filter (fun pi -> pi.GetIndexParameters().Length > 0)
    match propInfos with
    | [||] -> []
    | ps -> ps |> Array.toList    

let tryGetMethod (getStatic: bool) (rtype: Type) (name: string) : MethodInfo list =
    let bindingFlags = if getStatic then BindingFlags.Static ||| BindingFlags.Public else BindingFlags.Instance ||| BindingFlags.Public
    let methodInfos = rtype.GetMethods(bindingFlags) |> Array.filter (fun mi -> mi.Name = name)
    match methodInfos with
    | [||] -> []
    | ms -> ms |> Array.toList

type ResolvedMember =
    | ResolvedMethod of MethodInfo
    | ResolvedProperty of PropertyInfo
    | ResolvedIndexedProperty of PropertyInfo

let (|AllResolvedMethod|AllResolvedProperty|AllResolvedIndexedProperty|MixedResolution|EmptyResolution|) (ms: ResolvedMember list) =
    let foundResolvedMethod = ref false
    let foundResolvedProperty = ref false
    let foundResolvedIndexableProperty = ref false
    for m in ms do
        match m with
        | ResolvedMethod mi -> foundResolvedMethod := true
        | ResolvedProperty pi -> foundResolvedProperty := true
        | ResolvedIndexedProperty pi -> foundResolvedIndexableProperty := true
    match !foundResolvedMethod, !foundResolvedProperty, !foundResolvedIndexableProperty with
    | true, true, _ | true, _, true | _, true, true -> MixedResolution ms
    | false, false, false -> EmptyResolution
    | true, false, false -> AllResolvedMethod 
    | false, true, false -> AllResolvedProperty 
    | false, false, true -> AllResolvedIndexedProperty

let resolveMembersToExpr (o: obj) (ms: ResolvedMember list) = 
    match ms with
    | AllResolvedMethod -> AppliedMethod(o, ms |> List.map (function | ResolvedMethod mi -> mi)) |> InvokableExpr
    | AllResolvedProperty -> AppliedProperty(o, ms |> List.map (function | ResolvedProperty mi -> mi) |> List.head) // Note: Punting for now on the multi non-indexed property case
    | AllResolvedIndexedProperty -> AppliedIndexedProperty(o, ms |> List.map (function | ResolvedIndexedProperty mi -> mi))

let resolvedMemberToExpr (o: obj) (m: ResolvedMember) =
    match m with
    | ResolvedMethod(mi) -> InvokableExpr <| AppliedMethod(o, [mi])
    | ResolvedProperty(pi) -> AppliedProperty(o, pi)
    | ResolvedIndexedProperty(pi) -> AppliedIndexedProperty(o, [pi]) 

let rec resolveMember (rtype: System.Type) (memberName: string) : ResolvedMember list =
    [
        yield! tryGetProperty false rtype memberName |> List.map (ResolvedProperty)
        yield! tryGetMethod false rtype memberName |> List.map (ResolvedMethod)
        yield! tryGetIndexedProperty false rtype memberName |> List.map (ResolvedIndexedProperty)
        yield! rtype.GetInterfaces() |> Seq.collect (fun ii -> resolveMember ii memberName)
    ]
//    |> Option.tryResolve (fun () -> rtype.GetField(memberName) |> nullableToOption |> Option.map fieldToExpr)

let cachedResolveMember = 
    let inputToKey (rtype: System.Type, caseName) = rtype.FullName + "~" + caseName
    let resolveMember (rtype, caseName) = resolveMember rtype caseName
    memoizeBy inputToKey resolveMember

let fixFullName (ns: string) = 
    match ns.LastIndexOf('+') with
    | -1 -> ns
    | idx -> ns.Replace('+', '.')

let getModulesByNamespaceName (namespaces: string Set) =
    let possibleModules =
        AppDomain.CurrentDomain.GetAssemblies() 
        |> Seq.collect (fun a -> try a.GetTypes() with ex -> Array.empty) 
        |> Seq.filter (FSharpType.IsModule) 
    possibleModules |> Seq.filter (fun typ -> namespaces |> Set.contains (fixFullName typ.FullName))

let getContentsFromModule (modTyp: Type) = 
    // type MethodSig = ((obj array -> obj) * Type array) list
    seq {
        let mkRepfun expr = fun off len -> { Offset = off; Length = len; Expr = expr }
        for mi in modTyp.GetMembers() do
            match mi with
            | :? PropertyInfo as pi -> 
                match pi.GetIndexParameters() with
                | [||] -> yield pi.Name, mkRepfun (pi.GetValue(null) |> Obj)
                | prms -> yield pi.Name, mkRepfun <| AppliedIndexedProperty(null, [pi])
            // Actually Are F# Functions
            | :? MethodInfo as mi ->
                // Wraps F# functions in the internal lambda type to provide partial application support
                let lazyExpr off len =                 
                    let numparams = mi.GetParameters().Length
                    let mkRep expr = { Offset = off; Length = len; Expr = expr }  
                    let prmStrs = [ for i = 0 to numparams - 1 do yield "x" + (string i) ]
                    let innerExpr = SubExpression [ yield InvokableExpr <| AppliedMethod(null,  [mi]) |> mkRep; yield prmStrs |> List.map (fun s -> Unknown s |> mkRep) |> List.toArray |> Tuple |> mkRep]
                    { Params = prmStrs; Bindings = Map.empty; Contents = innerExpr |> mkRep} |> Lambda |> mkRep
                yield mi.Name, lazyExpr                    
            | :? FieldInfo as fi ->    yield fi.Name, mkRepfun ( fi.GetValue(null) |> Obj )
            | _ -> ()
    }

let getTypeByName (namespaces: string Set) (typename: string) = 
    AppDomain.CurrentDomain.GetAssemblies()  
    |> Seq.collect (fun a -> try a.GetTypes() with ex -> Array.empty) 
    |> Seq.filter (fun typ -> typ.Name = typename)
    |> Seq.filter (fun typ -> namespaces.Contains(typ.Namespace))
    |> Seq.toList

let resolveStatic (namespaces: string Set) (rtypename: string) (memberName: string) : ExprTypes list =
    match getTypeByName namespaces rtypename with
    | [] -> []
    | rtype :: [] -> 
        [
            yield! tryGetProperty true rtype memberName |> List.map (fun pi -> AppliedProperty(null, pi))
            let mi = tryGetMethod true rtype memberName in 
                        if mi |> List.isEmpty |> not then
                            yield InvokableExpr <| AppliedMethod(null, mi)
            let pi = tryGetIndexedProperty true rtype memberName in 
                        if pi |> List.isEmpty |> not then
                            yield AppliedIndexedProperty(null, pi)
        ]
//        |> Option.tryResolve (fun () -> rtype.GetField(memberName) |> nullableToOption |> Option.map fieldToExpr)
//        |> function | Some (objToExpr) -> Some (objToExpr null) | None -> failwith (sprintf "Member name of %s was ambiguous: %s" rtypename memberName)            
    | manytype -> failwith (sprintf "Type name was ambiguous: %s" rtypename) 

// Note: may not properly fail if types are loaded later, but I'm willing to sacrifice this for now in the name of complexity reduction
let cachedResolveStatic =
    let inputToKey (namespaces, typename, membername) = typename + "~" + membername
    let resolveMember (namespaces, typename, membername) = resolveStatic namespaces typename membername
    memoizeBy inputToKey resolveMember

let executeConstructor (namespaces: string Set) (rtypename: string) (args: obj []) : ExprTypes option =
    match getTypeByName namespaces rtypename with
    | [] -> None
    | rtype :: [] -> 
        let paramTypes = args |> Array.map (fun p -> p.GetType())
        rtype.GetConstructor paramTypes |> nullableToOption |> Option.map (fun ctor -> ctor.Invoke(args) |> Obj )         
    | manytype -> failwith (sprintf "Type name was ambiguous: %s" rtypename) 
   
let rec convertToTargetType (ttype: Type) (param: obj) = 
    match param with
    | null when ttype.IsGenericType && ttype.GetGenericTypeDefinition() = typedefof<_ option> -> FSharpType.MakeOptionNone (ttype.GetGenericArguments().[0]) |> Some
    | null -> Some null
    // Special Case For speed
    | :? (obj []) as objs when ttype = typeof<string[]> -> Array.ConvertAll(objs, fun v -> v :?> string) |> box |> Some 
    | :? (obj []) as objs when ttype = typeof<int64[]>  -> Array.ConvertAll(objs, fun v -> v :?> int64) |> box |> Some 
    | :? (obj []) as objs when ttype = typeof<int32[]>  -> Array.ConvertAll(objs, fun v -> v :?> int64 |> int) |> box |> Some 
    | _ when ttype.IsGenericType && ttype.GetGenericTypeDefinition() = typedefof<_ option> -> 
        let innerType = ttype.GetGenericArguments().[0]
        match convertToTargetType innerType param with
        | Some value -> FSharpType.MakeOptionSome innerType value |> Some
        | None -> None
    | DecomposeOption value -> convertToTargetType ttype param 
    | _ when ttype.IsGenericTypeDefinition -> Some param   
    | _ when ttype = typeof<IEnumerable> && param.GetType() = typeof<string> -> Some ([| param |] |> box)
    | _ when ttype.IsAssignableFrom(param.GetType()) -> Some param
    | _ when ttype = typeof<IEnumerable> -> Some ([| param |] |> box)
    | _ ->
        let des = TypeDescriptor.GetConverter(ttype)
        match des.CanConvertFrom(param.GetType()) with
        | true -> Some <| des.ConvertFrom(param)
        | false -> try Some <| System.Convert.ChangeType(param, ttype) with _ -> None

type MethodMatch =
    | PerfectMatch = 0 
    | GenericMatch = 1
    | LengthMatch = 2
    | NoMatch = 255

let getMethodMatch (mi: MethodInfo) (prms: ParameterInfo []) (args: obj []) =
    if prms.Length = args.Length then
        let zipped = Array.zip prms args
        if zipped |> Seq.forall (fun (t,a) -> a = null || t.ParameterType = a.GetType()) then MethodMatch.PerfectMatch
        elif mi.IsGenericMethod then MethodMatch.GenericMatch             
        else MethodMatch.LengthMatch
    else MethodMatch.NoMatch

let resolveGenericByName (prm: ParameterInfo) (arg: obj) = 
    let argType = arg.GetType()
    if prm.ParameterType.IsGenericParameter then // will be true for 'a (fully generic)               
        [|prm.Name, argType|] 
    elif prm.ParameterType.IsGenericType then // Will be true for 'a seq (nested generic)
        let names = prm.ParameterType.GenericTypeArguments |> Array.map (fun v -> v.Name)        
        let types = argType.GenericTypeArguments
        Array.zip names types
    else // Shouldn't get here
        failwith "Parameter was unexpectedly not Generic"

let resolveGenerics (mi: MethodInfo) (prms: ParameterInfo[]) (args: obj[]) =
    let gnamepos = mi.GetGenericArguments() |> Array.map (fun gi -> gi.Name, gi.GenericParameterPosition) |> Map.ofArray

    let resolvedGenerics = 
        Array.zip prms args
        |> Array.choose (fun (p,a) -> match p.ParameterType.ContainsGenericParameters with 
                                      | true -> resolveGenericByName p a |> Some
                                      | false -> None)
        |> Array.collect id
        |> Seq.groupBy fst |> Seq.map (fun (name, ptyp) -> name, ptyp |> Seq.head |> snd) 
        |> Seq.sortBy (fun (n,t) -> gnamepos |> Map.find n)

    let resolvedPrms = resolvedGenerics |> Seq.map (snd) |> Seq.toArray
    mi.MakeGenericMethod(resolvedPrms)

let convertArgs (prms: ParameterInfo[]) (args: obj[]) =
    Array.zip prms args |> Array.map (fun (prm, arg) -> convertToTargetType prm.ParameterType arg)
    |> Array.map (function | Some rParam -> rParam | None -> failwith (sprintf "Unable to resolve method parameters: %A -> %A" args prms))

let executeParameterizedMethod (o: obj) (sigs: MethodInfo list) (args: obj []) =
    let methodsWithParamArgs = sigs |> List.map (fun mi -> mi, mi.GetParameters())
    
    let orderedDispaches = 
        methodsWithParamArgs
        |> List.map (fun (mi, prms) -> mi, prms, getMethodMatch mi prms args)
        |> List.sortBy (fun (_,_,cls) -> int cls) // Lowest Is Bestest

    // Punt for now and only look at the top winner
    let mi, newargs = 
        match orderedDispaches with
        | (mi, prms, MethodMatch.PerfectMatch) :: _ -> mi, args
        | (mi, prms, MethodMatch.GenericMatch) :: _ -> resolveGenerics mi prms args, args
        | (mi, prms, MethodMatch.LengthMatch) :: _ -> mi, convertArgs prms args
        | __ -> failwithf "No match found for the given operation on %s" (o.GetType().FullName)

    mi.Invoke(o, newargs)

let resolveMembersByInstance (o: obj) (memberName: string) : ResolvedMember list option =
    if o = null then None
    else match cachedResolveMember (o.GetType(), memberName) with
         | [] -> failwith (sprintf "Unable to find member %s in object of type %s" memberName (o.GetType().Name))
         | resolvedMembers -> Some resolvedMembers //failwithf "Unable to decide which member of name %s to invoke on type %s" memberName (o.GetType().FullName)

let resolveInvokeByInstance (o: obj) (memberName: string) : ExprTypes option =
    match resolveMembersByInstance o memberName with
    | Some (ms) -> resolveMembersToExpr o ms |> Some
    | _ -> None

let rec resolveInvokeAtDepth (depth: int) (o: obj) (memberName: string) =
    match depth, box o with
    | 0, _ -> failwith "Unexpected Error: resolveInvokeAtDepth was called with a depth of 0"
    | 1, (:? IEnumerable as enum) -> 
         [ for e in enum do match resolveMembersByInstance e memberName with 
                                  | Some(mis) -> yield e, mis 
                                  | _ -> failwithf "Member of name %s not found on object %s" memberName (o.GetType().FullName) ] 
    | n, (:? IEnumerable as enum) ->  
         [ for e in enum do yield! resolveInvokeAtDepth (n - 1) e memberName ]
    | _, o -> failwith "Cannot invoke at depth with a non-enumerable object"

let rec resolveMembers (rtype: System.Type) (bindingflags: BindingFlags) =
    let properties = rtype.GetProperties(bindingflags)
    let methodCollections = rtype.GetMethods(bindingflags) 
                            |> Seq.filter (fun mi -> not (mi.IsSpecialName && (mi.Name.StartsWith("set_") || mi.Name.StartsWith("get_"))))
                            |> Seq.groupBy (fun mi -> mi.Name)
    let fields = rtype.GetFields(bindingflags)               
    seq {
        for prop in properties do  
            match prop.GetIndexParameters() with
            | [||] -> yield prop.Name, fun o -> AppliedProperty(o, prop) 
            | prms -> yield prop.Name, fun o -> AppliedIndexedProperty(o, [prop])
        for (name, methods) in methodCollections do 
            yield name, fun o -> let mis = methods |> Seq.toList in InvokableExpr <| AppliedMethod(o, mis) 
        //for field in fields do yield field.Name, fieldToExpr field
    }     

let rec inline convertSequence seq1 seq2 = 
    List.zip (seq1 |> Seq.toList) (seq2 |> Seq.toList) 
    |> List.map (fun (one, two) -> convertToSameType one two) 
    |> List.unzip |> (fun (l1, l2) -> l1 :> obj, l2 :> obj)

and convertToSameType (obj1: obj) (obj2: obj) : (obj * obj) = 
    try
        if obj1 <> null && obj2 <> null && obj1.GetType() = obj2.GetType() then obj1, obj2
        else
            let t1Des = TypeDescriptor.GetConverter(obj1.GetType())
            if t1Des.CanConvertFrom(obj2.GetType()) then obj1, t1Des.ConvertFrom(obj2)
            else match obj1, obj2 with
                    // Compares reference-typed lists
                    // NOTE: This will break with any version of .NET lower than 4.0
                    | (:? seq<obj> as ie1), (:? seq<obj> as ie2) -> convertSequence ie1 ie2
                    | (:? System.Collections.IEnumerable as ie1), (:? System.Collections.IEnumerable as ie2) -> convertSequence (ie1 |> Seq.cast) (ie2 |> Seq.cast)
                    | _ -> obj1, System.Convert.ChangeType(obj2, obj1.GetType())
    with _ -> failwith (sprintf "Failed to find a conversion for %A of %s and %A of %s" obj1 (obj1.GetType().ToString()) obj2 (obj2.GetType().ToString()))   

let executeUnitMethod (o: obj) (sigs: MethodInfo list) =
    sigs 
    |> List.map (fun mi -> mi, mi.GetParameters())
    |> List.tryFind (fun (mi, paramTypes) -> paramTypes.Length = 0)
    |> function
       | Some (mi, paramTypes) -> mi.Invoke(o, [||])
       | None -> failwithf "Unable to find a method with no parameters for unit invocation for: %A" o

let executeIndexer (o: obj) (sigs: PropertyInfo list) (param: obj) =
    sigs 
    |> List.map (fun (mi: PropertyInfo) -> mi, mi.GetIndexParameters() |> Array.map (fun p -> p.ParameterType))
    |> List.tryFind (fun (mi, paramTypes) -> paramTypes.Length = 1)
    |> Option.bind (fun (mi, paramTypes) -> 
        convertToTargetType (paramTypes.[0]) param 
        |> Option.map (fun converted -> mi, converted))
    |> Option.map (fun (mi, converted) -> mi.GetValue(o, [| converted |]))
    |> Option.map Returned

let resolveObjectIndexer (rtype: System.Type) =
    let indexers =
        seq {
            for itype in seq { yield rtype; yield! rtype.GetInterfaces() } do
                yield! itype.GetCustomAttributes(typeof<DefaultMemberAttribute>, true) 
                       |> Seq.map (fun attrib -> itype, (attrib :?> DefaultMemberAttribute).MemberName)
                yield! itype.GetProperties(BindingFlags.Instance ||| BindingFlags.Public)
                       |> Seq.filter (fun pi -> pi.GetIndexParameters().Length > 0)
                       |> Seq.map (fun pi -> itype, pi.Name)
        }
    let defaultMembers = rtype.GetCustomAttributes(typeof<DefaultMemberAttribute>, true) 
                         |> Seq.map (fun t -> rtype, (t :?> DefaultMemberAttribute).MemberName)
    match Seq.concat [indexers; defaultMembers] with
    | stuff when Seq.isEmpty stuff -> None 
    | names -> let typ, memberName = names |> Seq.nth 0     
               let pi = typ.GetProperty(memberName) in Some (pi, pi.GetIndexParameters())

let cachedResolveObjectIndexer = 
    let inputToKey (rtype: System.Type) = rtype.FullName
    let resolveValue rtype = resolveObjectIndexer rtype
    memoizeBy inputToKey resolveValue

let callIndexedProperty (target: obj) (indexVal: obj) =    
    if target = null then Some <| Obj null
    else
        let ttype = target.GetType()
        match target, indexVal with
        | (:? Array as arr),          (:? int64 as idx) -> 
            if idx < 0L || idx >= int64 arr.Length then Returned null |> Some
            else arr.GetValue(idx) |> Returned |> Some
        | (:? IEnumerable as enumer), (:? int64 as idx) -> 
            if idx < 0L then Returned null |> Some
            else enumer |> Seq.cast<obj> |> Seq.nth (int idx) |> Returned |> Some  
        | _ ->
            match cachedResolveObjectIndexer ttype with
            | None -> None
            | Some (propInfo, paramInfos) -> 
                match paramInfos with
                | [| |] -> failwith (sprintf "Expected an indexed object, but got non-indexed: %s" ttype.FullName)
                | [| arg |] -> 
                    match convertToTargetType (arg.ParameterType) indexVal with
                    | Some (converted) -> Some <| (Returned (propInfo.GetValue(target, [| converted |])))
                    | None -> failwith (sprintf "No conversion found from %s of %s to %s" (string indexVal) (indexVal.GetType().ToString()) (arg.ParameterType.ToString()))                
                | other -> failwith (sprintf "MultiIndexed objects are not currently supported: %s" ttype.FullName)


let compareAsSameType obj1 obj2 func =
    let cobj1, cobj2 = convertToSameType obj1 obj2     
    func cobj1 cobj2
     
let rec objectsEqualInner (obj1: obj) (obj2: obj) =
    match obj1, obj2 with
    | null, null -> true 
    | (:? seq<obj> as seq1), (:? seq<obj> as seq2) -> 
        if Seq.length seq1 <> Seq.length seq2 then false
        else Seq.zip seq1 seq2 |> Seq.forall (fun (o1, o2) -> objectsEqualInner o1 o2)
    | null, _ -> false
    | _, null -> false
    | obj1, obj2 -> compareAsSameType obj1 obj2 (fun o1 o2 -> o1.Equals(o2))

let objectsEqual (obj1: obj) (obj2: obj) = 
    box <| objectsEqualInner obj1 obj2

let objectsNotEqual (obj1: obj) (obj2: obj) =  
    box <| (not (objectsEqualInner obj1 obj2))

let compareObjectsInner op =
    fun (obj1: obj) (obj2: obj) ->
        match obj1, obj2 with
        | null, null -> false
        | null, _ | _, null -> false
        | (:? IComparable as comp1), (:? IComparable as comp2) when obj1.GetType() = obj2.GetType() -> op comp1 comp2
        | (:? IComparable as comp1), obj2 -> compareAsSameType comp1 obj2 (fun o1 o2 -> op (o1 :?> IComparable) (o2 :?> IComparable))
        | _ -> failwith (sprintf "Unable to compare %A and %A" obj1 obj2)

let compareObjects op (obj1: obj) (obj2: obj) =
    compareObjectsInner op obj1 obj2 |> (fun res -> box res)

let notOp (obj1: obj) =
    match obj1 with
    | null -> null
    | (:? bool as b) -> box (not b)
    | w1 -> failwith (sprintf "Unexcepted argument for 'not' operation: %A" w1) 

let andOp (obj1: obj) (obj2: obj) =
    match obj1, obj2 with 
    | null, _ | _, null -> null
    | (:? bool as b1), (:? bool as b2) -> box (b1 && b2)
    | w1, w2 -> failwith (sprintf "Unexcepted arguments for 'and' operation: %A and %A" w1 w2)  

let orOp (obj1: obj) (obj2: obj) =
    match obj1, obj2 with 
    | null, _ | _, null -> null
    | (:? bool as b1), (:? bool as b2) -> box (b1 || b2)
    | w1, w2 -> failwith (sprintf "Unexcepted arguments for 'or' operation: %A or %A" w1 w2)  


let addObjects : (unit -> (obj -> obj -> obj)) = 
    let addExpr = (fun exp1 exp2 -> Expression.AddChecked(exp1, exp2))
    let specialCases = [| typeof<string>, typeof<string>, fun (o1: obj) (o2: obj) -> (o1 :?> string) + (o2 :?> string) |> box |]
    generateCachedNumericOperator addExpr specialCases

let subObjects : (unit -> (obj -> obj -> obj)) = 
    let subExpr = (fun exp1 exp2 -> Expression.SubtractChecked(exp1, exp2))
    let specialCases = [| |]
    generateCachedNumericOperator subExpr specialCases

let divideObjects : (unit -> (obj -> obj -> obj)) = 
    let divExpr = (fun exp1 exp2 -> Expression.Divide(exp1, exp2))
    let specialCases = [| |]
    generateCachedNumericOperator divExpr specialCases

let multObjects : (unit -> (obj -> obj -> obj)) = 
    let mulExpr = (fun exp1 exp2 -> Expression.MultiplyChecked(exp1, exp2))
    let specialCases = [| |]
    generateCachedNumericOperator mulExpr specialCases

let bitwiseOrObjects : (unit -> (obj -> obj -> obj)) = 
    let orExpr = (fun exp1 exp2 -> Expression.Or(exp1, exp2))
    let specialCases = [| |]
    generateCachedNumericOperator orExpr specialCases

let bitwiseAndObjects : (unit -> (obj -> obj -> obj)) = 
    let andExpr = (fun exp1 exp2 -> Expression.And(exp1, exp2))
    let specialCases = [| |]
    generateCachedNumericOperator andExpr specialCases

let objToEnumerable (obj: obj) =
    match obj with
    | null -> Array.empty |> Seq.cast<obj>
    | (:? String as str1) -> [|str1|] |> Seq.cast<obj>
    | (:? IEnumerable as en1) -> en1 |> Seq.cast<obj>
    | o -> [| o |] :> IEnumerable<obj>

let emptySingletonArray (enum: obj array) =
    match enum.Length with
    | 0 -> null |> box
    | 1 -> enum.[0] |> box
    | _ -> enum |> box

let unionObjects (obj1: obj) (obj2: obj) = 
    (objToEnumerable obj1, objToEnumerable obj2)
    |> (fun (t1,t2) -> (t1 |> Seq.cast<obj>).Union((t2 |> Seq.cast<obj>))) |> Seq.toArray |> emptySingletonArray

let intersectObjects (obj1: obj) (obj2: obj) = 
    (objToEnumerable obj1, objToEnumerable obj2)
    |> (fun (t1, t2) -> t1.Intersect(t2)) |> Seq.toArray |> emptySingletonArray

let doObjectsIntersect (obj1: obj) (obj2: obj) =
    (objToEnumerable obj1, objToEnumerable obj2)
    |> (fun (t1, t2) -> t1 |> Seq.exists (fun i1 -> t2 |> Seq.exists (fun i2 -> objectsEqualInner i1 i2))) |> box

let isSubsetOf (obj1: obj) (obj2: obj) = 
    (objToEnumerable obj1, objToEnumerable obj2)
    |> (fun (t1, t2) -> t1 |> Seq.forall (fun i1 -> t2 |> Seq.exists (fun i2 -> objectsEqualInner i1 i2))) |> box

let isSupersetOf (obj1: obj) (obj2: obj) = 
    (objToEnumerable obj2, objToEnumerable obj1)
    |> (fun (t1, t2) -> t1 |> Seq.forall (fun i1 -> t2 |> Seq.exists (fun i2 -> objectsEqualInner i1 i2))) |> box
