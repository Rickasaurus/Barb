module Barb.Interop

open System
open System.Text
open System.Text.RegularExpressions
open System.Reflection
open System.ComponentModel
open System.Linq
open System.Collections.Concurrent
open System.Collections.Generic

open Barb.Helpers
open Barb.Representation

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
            if gt = genericOptionType then
                Some <| t.InvokeMember("Value", bindingFlags, null, o, Array.empty)
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
    | (:? float as num) -> Some (decimal num :> obj)
    | (:? single as num) -> Some (decimal num :> obj)
    | value -> None

let rec resolveResultType (output: obj) = 
    match output with
    | null -> Obj null
    | DecomposeOption contents -> resolveResultType contents
    | SupportedNumberType contents -> Obj contents
    | other -> Obj other

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
                    prop.GetValue(parentResult, null) |> Returned
            yield fullName, getPropFunc
        for (name, methods) in methodCollections do
            let resolvedMethods = methods |> Seq.toList |> List.map (fun mi -> mi, mi.GetParameters() |> Array.map (fun pi -> pi.ParameterType))
            let func = 
                fun (instance:obj) -> 
                    let methodOverloads =
                        [
                            for mi, prms in resolvedMethods do
                                let resolver = 
                                    fun (args) -> 
                                        let parentResult = getter instance
                                        mi.Invoke(parentResult, args)
                                yield resolver, prms
                        ]
                    Method methodOverloads
            yield name, func
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
    else
        let des = TypeDescriptor.GetConverter(ttype)
        match des.CanConvertFrom(param.GetType()) with
        | true -> Some <| des.ConvertFrom(param)
        | false -> try Some <| System.Convert.ChangeType(param, ttype) with _ -> None





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

let executeIndexer (sigs: MethodSig) (param: obj) =
    sigs
    |> List.tryFind (fun (exec, paramTypes) -> paramTypes.Length = 1)
    |> Option.bind (fun (exec, paramTypes) -> 
        convertToTargetType (paramTypes.[0]) param 
        |> Option.map (fun converted -> exec, converted))
    |> Option.map (fun (exec, converted) -> exec [| converted |])
    |> Option.map Returned

let executeParameterizedMethod (sigs: MethodSig) (args: obj) =
    let arrayArgs =
        match args with
        | :? (obj seq) as tuple -> tuple |> Seq.toArray 
        | _ as o -> [| o |]
    sigs
    |> List.tryFind (fun (exec, paramTypes) -> paramTypes.Length = arrayArgs.Length)
    |> Option.map (fun (exec, paramTypes) -> 
        Array.zip paramTypes arrayArgs
        |> Array.map (fun (tType, param) -> convertToTargetType tType param)
        |> Array.map (function | Some rParam -> rParam | None -> failwith (sprintf "Unable to resolve method parameters: %A -> %A" arrayArgs paramTypes))
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
                | None -> failwith (sprintf "No conversion found from %s of %s to %s" (string indexVal) (indexVal.GetType().ToString()) (arg.ParameterType.ToString()))                
            | other -> failwith (sprintf "MultiIndexed objects are not currently supported: %s" ttype.FullName)


let compareAsSameType obj1 obj2 func =
    let cobj1, cobj2 = convertToSameType obj1 obj2     
    func cobj1 cobj2
     
let objectsEqualInner (obj1: obj) (obj2: obj) = 
    if obj1 = null && obj2 = null then true
    elif obj1 = null || obj2 = null then false
    else compareAsSameType obj1 obj2 (fun o1 o2 -> o1.Equals(o2))

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

open System.Numerics

let addObjects (obj1: obj) (obj2: obj) =
    if obj1 = null || obj2 = null then failwith "Unexpected null in addition"
    match obj1, obj2 with
    | (:? decimal as d1), (:? decimal as d2) -> (d1 + d2) :> obj
    | (:? int64 as b1), (:? int64 as b2) -> (b1 + b2) :> obj
    | (:? decimal as d1), (:? int64 as b2) -> (d1 + decimal b2) :> obj
    | (:? int64 as b1), (:? decimal as d2) -> (decimal b1 + d2) :> obj
    | _ -> failwith (sprintf "Cannot add %A of %s and %A of %s" obj1 (obj1.GetType().ToString()) obj2 (obj2.GetType().ToString()))

let subObjects (obj1: obj) (obj2: obj) =
    if obj1 = null || obj2 = null then failwith "Unexpected null in subtraction"
    match obj1, obj2 with
    | (:? decimal as d1), (:? decimal as d2) -> (d1 - d2) :> obj
    | (:? int64 as b1), (:? int64 as b2) -> (b1 - b2) :> obj
    | (:? decimal as d1), (:? int64 as b2) -> (d1 - decimal b2) :> obj
    | (:? int64 as b1), (:? decimal as d2) -> (decimal b1 - d2) :> obj
    | _ -> failwith (sprintf "Cannot subtract %A of %s and %A of %s" obj1 (obj1.GetType().ToString()) obj2 (obj2.GetType().ToString()))

let divideObjects (obj1: obj) (obj2: obj) =
    if obj1 = null || obj2 = null then failwith "Unexpected null in division"
    match obj1, obj2 with
    | (:? decimal as d1), (:? decimal as d2) -> (d1 / d2) :> obj
    | (:? int64 as b1), (:? int64 as b2) -> (b1 / b2) :> obj
    | (:? decimal as d1), (:? int64 as b2) -> (d1 / decimal b2) :> obj
    | (:? int64 as b1), (:? decimal as d2) -> (decimal b1 / d2) :> obj
    | _ -> failwith (sprintf "Cannot divide %A of %s and %A of %s" obj1 (obj1.GetType().ToString()) obj2 (obj2.GetType().ToString()))

let multObjects (obj1: obj) (obj2: obj) =
    if obj1 = null || obj2 = null then failwith "Unexpected null in multiplication"
    match obj1, obj2 with
    | (:? decimal as d1), (:? decimal as d2) -> (d1 * d2) :> obj
    | (:? int64 as b1), (:? int64 as b2) -> (b1 * b2) :> obj
    | (:? decimal as d1), (:? int64 as b2) -> (d1 * decimal b2) :> obj
    | (:? int64 as b1), (:? decimal as d2) -> (decimal b1 * d2) :> obj
    | _ -> failwith (sprintf "Cannot multiply %A of %s and %A of %s" obj1 (obj1.GetType().ToString()) obj2 (obj2.GetType().ToString()))

        
        
