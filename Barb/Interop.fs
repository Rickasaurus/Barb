module Barb.Interop

open System
open System.Text
open System.Text.RegularExpressions
open System.Reflection
open System.ComponentModel
open System.Linq
open System.Linq.Expressions
open System.Collections
open System.Collections.Concurrent
open System.Collections.Generic
open System.Runtime.CompilerServices
open System.Numerics  

open Barb.Helpers
open Barb.Representation


// Left is higher precedence
// TODO/IDEA: Use ranges of values ot determine overlap and generate correct unifications for all pairs
let orderOfNumericConversions = 
    [| TypeCode.Double; TypeCode.Single;
       TypeCode.Int64; TypeCode.Int32; TypeCode.Int16; TypeCode.SByte;
       TypeCode.UInt64; TypeCode.UInt32; TypeCode.UInt16; TypeCode.Char; TypeCode.Byte |]

let expConv prm typ = Expression.Convert(prm, typ)

type BinaryExprFunc = Expression -> Expression -> BinaryExpression
type BinaryExprSpecialCases = (Type * Type * (obj -> obj -> obj)) []
let genDynamicOperatorFunc (binExpr: BinaryExprFunc) (specialCases:BinaryExprSpecialCases) (typ1: Type) (typ2: Type) : obj -> obj -> obj =
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
//    | ConvertSequence contents -> Obj contents
    | other -> Obj other

let fieldToExpr (fld: FieldInfo) =
    fun (obj: obj) ->
        fld.GetValue(obj, null) |> Returned
    
let propertyToExpr (prop: PropertyInfo) =
    match prop.GetIndexParameters() with
    | [||] -> (fun obj -> prop.GetValue(obj, null) |> Returned)
    | prms ->
        let typeArgs = prms |> Array.map (fun pi -> pi.ParameterType)
        (fun (obj: obj) -> [(fun args -> prop.GetValue(obj, args)), typeArgs] |> IndexedProperty)

let overloadedMethodToExpr (methods: MethodInfo seq) =
    let methodsWithPrms = methods |> Seq.map (fun mi -> mi, mi.GetParameters() |> Array.map (fun pi -> pi.ParameterType)) |> Seq.toList
    fun (instance: obj) -> 
        let methodOverloads =
            [
                for mi, prms in methodsWithPrms do
                    let resolver = 
                        fun (args) -> 
                            mi.Invoke(instance, args)
                    yield resolver, prms
            ]
        Method methodOverloads

let rec resolveMember (rtype: System.Type) (memberName: string) : (obj -> ExprTypes) option =
    rtype.GetProperty(memberName) |> nullableToOption |> Option.map propertyToExpr
    |> Option.tryResolve (fun () -> match rtype.GetMethods() |> Array.filter (fun mi -> mi.Name = memberName) with | [||] -> None | methods -> methods |> overloadedMethodToExpr |> Some)
    |> Option.tryResolve (fun () -> rtype.GetField(memberName) |> nullableToOption |> Option.map fieldToExpr)
    |> Option.tryResolve (fun () -> rtype.GetInterfaces() |> Seq.tryPick (fun i -> resolveMember i memberName))

let cachedResolveMember = 
    let inputToKey (rtype: System.Type, caseName) = rtype.FullName + "~" + caseName
    let resolveMember (rtype, caseName) = resolveMember rtype caseName
    memoizeBy inputToKey resolveMember

let getTypeByName (namespaces: string Set) (typename: string) = 
    AppDomain.CurrentDomain.GetAssemblies()  
    |> Seq.collect (fun a -> a.GetTypes())
    |> Seq.filter (fun typ -> typ.Name = typename)
    |> Seq.filter (fun typ -> namespaces.Contains(typ.Namespace))
    |> Seq.toList

let resolveStatic (namespaces: string Set) (rtypename: string) (memberName: string) : ExprTypes option =
    match getTypeByName namespaces rtypename with
    | [] -> None
    | rtype :: [] ->        
        rtype.GetProperty(memberName) |> nullableToOption |> Option.map propertyToExpr
        |> Option.tryResolve (fun () -> match rtype.GetMethods() |> Array.filter (fun mi -> mi.Name = memberName) with | [||] -> None | methods -> methods |> overloadedMethodToExpr |> Some)
        |> Option.tryResolve (fun () -> rtype.GetField(memberName) |> nullableToOption |> Option.map fieldToExpr)
        |> function | Some (objToExpr) -> Some (objToExpr null) | None -> failwith (sprintf "Member name of %s was ambiguous: %s" rtypename memberName)            
    | manytype -> failwith (sprintf "Type name was ambiguous: %s" rtypename) 

// Note: may not properly fail if types are loaded later, but I'm willing to sacrifice this for now in the name of complexity reduction
let cachedResolveStatic =
    let inputToKey (namespaces, typename, membername) = typename + "~" + membername
    let resolveMember (namespaces, typename, membername) = resolveStatic namespaces typename membername
    memoizeBy inputToKey resolveMember

let executeConstructor (namespaces: string Set) (rtypename: string) (parameters: obj) : ExprTypes option =
    match getTypeByName namespaces rtypename with
    | [] -> None
    | rtype :: [] -> 
        let args = convertPotentiallyTupled parameters
        let paramTypes = args |> Array.map (fun p -> p.GetType())
        rtype.GetConstructor paramTypes |> nullableToOption |> Option.map (fun ctor -> ctor.Invoke(args) |> Obj )         
    | manytype -> failwith (sprintf "Type name was ambiguous: %s" rtypename) 

let resolveInvoke (o: obj) (memberName: string) =
    if o = null then None
    else match cachedResolveMember (o.GetType(), memberName) with
         | Some (resolvedMember) -> resolvedMember o |> Some
         | None -> failwith (sprintf "Unable to lookup specified member %s in object %s" memberName (o.GetType().Name))

let rec resolveMembers (rtype: System.Type) (bindingflags: BindingFlags) =
    let properties = rtype.GetProperties(bindingflags)
    let methodCollections = rtype.GetMethods(bindingflags) |> Seq.groupBy (fun mi -> mi.Name)
    let fields = rtype.GetFields(bindingflags)               
    seq {
        for prop in properties do yield prop.Name, propertyToExpr prop
        for (name, methods) in methodCollections do yield name, overloadedMethodToExpr methods
        for field in fields do yield field.Name, fieldToExpr field
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

let convertToTargetType (ttype: Type) (param: obj) = 
    if param = null then Some null
    elif ttype.IsGenericTypeDefinition then Some param
    elif ttype = typeof<IEnumerable> && param.GetType() = typeof<string> then Some ([| param |] |> box)
    elif ttype.IsAssignableFrom(param.GetType()) then Some param
    elif ttype = typeof<IEnumerable> then Some ([| param |] |> box)
    else
        let des = TypeDescriptor.GetConverter(ttype)
        match des.CanConvertFrom(param.GetType()) with
        | true -> Some <| des.ConvertFrom(param)
        | false -> try Some <| System.Convert.ChangeType(param, ttype) with _ -> None

let executeUnitMethod (sigs: MethodSig) =
    sigs 
    |> List.tryFind (fun (exec, paramTypes) -> paramTypes.Length = 0)
    |> Option.bind (fun (exec, paramTypes) -> Some <| exec Array.empty)
    |> Option.map Returned

let executeIndexer (sigs: MethodSig) (param: obj) =
    sigs
    |> List.tryFind (fun (exec, paramTypes) -> paramTypes.Length = 1)
    |> Option.bind (fun (exec, paramTypes) -> 
        convertToTargetType (paramTypes.[0]) param 
        |> Option.map (fun converted -> exec, converted))
    |> Option.map (fun (exec, converted) -> exec [| converted |])
    |> Option.map Returned

let executeParameterizedMethod (sigs: MethodSig) (args: obj) =
    let arrayArgs = convertPotentiallyTupled args
    sigs
    |> List.tryFind (fun (exec, paramTypes) -> paramTypes.Length = arrayArgs.Length)
    |> Option.tryResolve (fun () -> sigs |> List.tryFind (fun (exec, paramTypes) -> paramTypes.Length = 1))
    |> Option.map (fun (exec, paramTypes) -> 
        Array.zip paramTypes arrayArgs
        |> Array.map (fun (tType, param) -> convertToTargetType tType param)
        |> Array.map (function | Some rParam -> rParam | None -> failwith (sprintf "Unable to resolve method parameters: %A -> %A" arrayArgs paramTypes))
        |> fun rParams -> exec rParams)
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
    | (:? bool as b) -> box (not b)
    | w1 -> failwith (sprintf "Unexcepted argument for 'not' operation: %A" w1) 

let andOp (obj1: obj) (obj2: obj) =
    match obj1, obj2 with 
    | (:? bool as b1), (:? bool as b2) -> box (b1 && b2)
    | w1, w2 -> failwith (sprintf "Unexcepted arguments for 'and' operation: %A and %A" w1 w2)  

let orOp (obj1: obj) (obj2: obj) =
    match obj1, obj2 with 
    | (:? bool as b1), (:? bool as b2) -> box (b1 || b2)
    | w1, w2 -> failwith (sprintf "Unexcepted arguments for 'or' operation: %A or %A" w1 w2)  


let addObjects () = 
    let addExpr = (fun exp1 exp2 -> Expression.Add(exp1, exp2))
    let specialCases = [| typeof<string>, typeof<string>, fun (o1: obj) (o2: obj) -> (o1 :?> string) + (o2 :?> string) |> box |]
    let genFunc = genDynamicOperatorFunc addExpr specialCases
    let cache: (obj -> obj -> obj) Option ref = ref None
    //TODO: Hidden cache goes here. Should assume the same each time.
    fun obj1 obj2 -> 
        let result = try !cache |> Option.bind (fun func -> func obj1 obj2 |> Some) with ex -> None
        match result with
        | Some(res) -> res
        | None -> let newFunc = genFunc (obj1.GetType()) (obj2.GetType()) 
                  do cache := newFunc |> Some
                  newFunc obj1 obj2 

let subObjects (obj1: obj) (obj2: obj) =
    if obj1 = null || obj2 = null then failwith "Unexpected null in subtraction"
    match obj1, obj2 with
    | (:? float as d1), (:? float as d2) -> (d1 - d2) :> obj
    | (:? int64 as b1), (:? int64 as b2) -> (b1 - b2) :> obj
    | (:? float as d1), (:? int64 as b2) -> (d1 - float b2) :> obj
    | (:? int64 as b1), (:? float as d2) -> (float b1 - d2) :> obj
    | _ -> failwith (sprintf "Cannot subtract %A of %s and %A of %s" obj1 (obj1.GetType().ToString()) obj2 (obj2.GetType().ToString()))

let divideObjects (obj1: obj) (obj2: obj) =
    if obj1 = null || obj2 = null then failwith "Unexpected null in division"
    match obj1, obj2 with
    | (:? float as d1), (:? float as d2) -> (d1 / d2) :> obj
    | (:? int64 as b1), (:? int64 as b2) -> (b1 / b2) :> obj
    | (:? float as d1), (:? int64 as b2) -> (d1 / float b2) :> obj
    | (:? int64 as b1), (:? float as d2) -> (float b1 / d2) :> obj
    | _ -> failwith (sprintf "Cannot divide %A of %s and %A of %s" obj1 (obj1.GetType().ToString()) obj2 (obj2.GetType().ToString()))

let multObjects (obj1: obj) (obj2: obj) =
    if obj1 = null || obj2 = null then failwith "Unexpected null in multiplication"
    match obj1, obj2 with
    | (:? float as d1), (:? float as d2) -> (d1 * d2) :> obj
    | (:? int64 as b1), (:? int64 as b2) -> (b1 * b2) :> obj
    | (:? float as d1), (:? int64 as b2) -> (d1 * float b2) :> obj
    | (:? int64 as b1), (:? float as d2) -> (float b1 * d2) :> obj
    | _ -> failwith (sprintf "Cannot multiply %A of %s and %A of %s" obj1 (obj1.GetType().ToString()) obj2 (obj2.GetType().ToString()))

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
