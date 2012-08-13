(******************************************************************************

Copyright (C) 2011 IntelliFactory

This program is free software: you can redistribute it and/or modify
it under the terms of the GNU General Public License as published by
the Free Software Foundation, either version 3 of the License, or
(at your option) any later version.

This program is distributed in the hope that it will be useful,
but WITHOUT ANY WARRANTY; without even the implied warranty of
MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
GNU General Public License for more details.

You should have received a copy of the GNU General Public License
along with this program.  If not, see <http://www.gnu.org/licenses/>.

******************************************************************************)

[<AutoOpen>]
module internal IntelliFactory.Documentation.Extensions

open System
open System.Collections.Generic
open System.IO
open System.Runtime.CompilerServices
open System.Reflection
open Mono.Cecil

type private CMA    = CompilationMappingAttribute
type private CGA    = CompilerGeneratedAttribute
type private CSNA   = CompilationSourceNameAttribute
type private CRA    = CompilationRepresentationAttribute
type private CRF    = CompilationRepresentationFlags
type private SCF    = SourceConstructFlags
type private OA     = OptionalArgumentAttribute
type private CACA   = CompilationArgumentCountsAttribute

[<AutoOpen>]
module private Util =

    let HasAttribute<'T when 'T :> Attribute>
        (x: ICustomAttributeProvider) =
            let s = typeof<'T>.FullName
            x.HasCustomAttributes &&
            x.CustomAttributes
            |> Seq.exists (fun x ->
                x.AttributeType.FullName = s)

    let GetAttribute<'T when 'T :> Attribute>
        (x: ICustomAttributeProvider) =
            let s = typeof<'T>.FullName
            x.CustomAttributes
            |> Seq.find (fun x ->
                x.AttributeType.FullName = s)

    let TryGetAttribute<'T when 'T :> Attribute>
        (x: ICustomAttributeProvider) =
            let s = typeof<'T>.FullName
            x.CustomAttributes
            |> Seq.tryFind (fun x ->
                x.AttributeType.FullName = s)

    let GetFSharpName (x: MemberReference) =
        use out = new StringWriter()
        let writeName () =
            match x with
            | :? TypeReference as x ->
                match TypeAbbreviations.Abbreviate x.FullName with
                | Some x ->
                    out.Write x
                | None ->
                    let name = x.Name
                    if x.HasGenericParameters then
                        match name.LastIndexOf '`' with
                        | -1 -> name
                        | k  -> name.Substring(0, k)
                        |> out.Write
                    else
                        out.Write name
            | _ ->
                out.Write x.Name
        match x :> obj with
        | :? ICustomAttributeProvider as p ->
            match TryGetAttribute<CSNA> p with
            | Some a ->
                a.ConstructorArguments.[0].Value :?> string
                |> out.Write
            | None ->
                match TryGetAttribute<CRA> p with
                | Some a ->
                    let v = a.ConstructorArguments.[0].Value
                    if (CRF.ModuleSuffix &&& downcast v) = CRF.ModuleSuffix then
                        x.Name.Substring(0, x.Name.Length - "Module".Length)
                        |> out.Write
                    else
                        writeName ()
                | None ->
                    writeName ()
        | _ ->
            writeName ()
        match x :> obj with
        | :? IGenericParameterProvider as x ->
            if x.HasGenericParameters then
                let ps = Seq.toList x.GenericParameters
                out.Write "<"
                out.Write "'"
                out.Write ps.Head.Name
                for x in ps.Tail do
                    out.Write ",'"
                    out.Write x.Name
                out.Write ">"
        | _ ->
            ()
        out.ToString()

module Record =
    type private FSV = Microsoft.FSharp.Reflection.FSharpValue
    type private FST = Microsoft.FSharp.Reflection.FSharpType

    let private Flags =
        BindingFlags.Public ||| BindingFlags.NonPublic

    let Default<'T> : 'T =
        let cons = FSV.PreComputeRecordConstructor(typeof<'T>, Flags)
        let n = FST.GetRecordFields(typeof<'T>,Flags).Length
        cons [| for x in 1 .. n -> null |] :?> 'T

    let Patch<'T1,'T2> (fields: 'T1) (result: 'T2) =
        let cons = FSV.PreComputeRecordConstructor(typeof<'T2>, Flags)
        let r1 = FSV.PreComputeRecordReader(typeof<'T1>, Flags)
        let r2 = FSV.PreComputeRecordReader(typeof<'T2>, Flags)
        let f1 = FST.GetRecordFields(typeof<'T1>,Flags)
        let f2 = FST.GetRecordFields(typeof<'T2>,Flags)
        let d = Dictionary()
        for (f, v) in Seq.zip f2 (r2 result) do
            d.[(f.Name, f.PropertyType)] <- v
        for (f, v) in Seq.zip f1 (r1 fields) do
            d.[(f.Name, f.PropertyType)] <- v
        cons [| for f in f2 -> d.[(f.Name, f.PropertyType)] |] :?> 'T2

type ICustomAttributeProvider with

    member this.HasAttribute<'T when 'T :> Attribute>() =
        HasAttribute<'T> this

    member this.GetAttribute<'T when 'T :> Attribute>() =
        GetAttribute<'T> this

    member this.TryGetAttribute<'T when 'T :> Attribute>() =
        TryGetAttribute<'T> this

    member this.IsCompilerGenerated =
        this.HasAttribute<CGA>()


type ParameterDefinition with
    member this.IsOptionalParameter =
        this.HasAttribute<OA>()

type MethodDefinition with

    member this.FSharpName =
        GetFSharpName this
    
    member this.TryGetCompilationArgumentCounts() = 
        match this.TryGetAttribute<CACA>() with
        | Some attr ->
            let args = attr.ConstructorArguments.[0].Value :?> Mono.Cecil.CustomAttributeArgument []
            let counts = [| for arg in args -> arg.Value :?> int|] 
            Some counts
        | None -> None

    member this.IsUnionCase =
        match this.TryGetAttribute<CMA>() with
        | Some attr ->
            let a = attr.ConstructorArguments.[0]
            match a.Value :?> _ with
            | Core.SourceConstructFlags.UnionCase ->
                true
            | _ ->
                false
        | None ->
            false

    member this.GetUnionCaseTag() : int= 
        match this.TryGetAttribute<CMA>() with
        | Some attr ->
            let a = attr.ConstructorArguments.[0]
            match a.Value :?> _ with
            | Core.SourceConstructFlags.UnionCase ->
                downcast attr.ConstructorArguments.[1].Value
            | _ ->
                failwith "Method should be union case creator"
        | None ->
            failwith "Method should be union case creator"
        

    member this.IsOperator = 
        this.IsSpecialName && this.Name.StartsWith "op_"

type PropertyDefinition with

    member this.FSharpName =
        GetFSharpName this

    member this.IsStatic =
        this.GetMethod <> null
        && this.GetMethod.IsStatic
        || this.SetMethod <> null
        && this.SetMethod.IsStatic

    member this.IsPublic =
        this.GetMethod <> null
        && this.GetMethod.IsPublic
        || this.SetMethod <> null
        && this.SetMethod.IsPublic

    member this.IsField =
        match this.TryGetAttribute<CMA>() with
        | Some attr ->
            let a = attr.ConstructorArguments.[0]
            match a.Value :?> _ with
            | Core.SourceConstructFlags.Field ->
                true
            | _ ->
                false
        | None ->
            false

type FieldDefinition with

    member this.FSharpName =
        GetFSharpName this

type TypeDefinition with

    member this.FSharpName =
        GetFSharpName this

    member this.UnionCaseName =
        if this.Name.StartsWith "_" then
            this.Name.Substring 1
        else
            this.Name

    member this.HasUseNullAsTrueValue = 
        match TryGetAttribute<CRA> this with
        | Some attr ->
            let v = attr.ConstructorArguments.[0].Value
            (CRF.UseNullAsTrueValue &&& downcast v) = CRF.UseNullAsTrueValue        
        | None -> false

    member this.FSharpEntity =
        match this.TryGetAttribute<CMA>() with
        | Some attr ->
            let a = attr.ConstructorArguments.[0]
            Some (a.Value :?> SourceConstructFlags)
        | None ->
            None

    member this.IsModule =
        this.FSharpEntity = Some SCF.Module

    member this.IsRecord =
        this.FSharpEntity = Some SCF.RecordType

    member this.IsUnion =
        this.FSharpEntity = Some SCF.SumType

type TypeReference with
    member this.FSharpName =
        GetFSharpName this
