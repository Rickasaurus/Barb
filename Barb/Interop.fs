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