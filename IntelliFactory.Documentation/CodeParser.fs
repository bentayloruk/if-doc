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

namespace IntelliFactory.Documentation

/// Parses Mono.Cecil assembly definitions into documentation CodeModel.
module CodeParser =
    open System
    open System.Collections.Generic
    open System.IO
    open Mono.Cecil
    module CM = CodeModel

    type private Metadata =
        {
            Id : XmlDoc.Id
            Documentation : option<CM.Documentation>
        }

    /// Parses a given Mono.Cecil-loaded assembly to CodeModel types.
    let ParseAssembly (assembly: AssemblyDefinition) 
                      (documentation: XmlDoc.Document) : CM.Assembly =

        let rec parseTR (t: TypeReference) : CM.TypeReference =
            use out = new StringWriter()
            let rec loop (t: TypeReference) =
                match t.DeclaringType with
                | null ->
                    out.Write t.Namespace
                    out.Write '.'
                    out.Write t.Name
                | dT   ->
                    loop dT
                    out.Write '.'
                    out.Write t.Name
            loop t
            let name = out.ToString()
            let noGen (x: string) =
                match x.LastIndexOf '`' with
                | -1 -> x
                | k  -> x.Substring(0, k)
            {
                Assembly =
                    let asm = 
                        try t.Resolve() 
                        with :? System.IO.FileNotFoundException -> null 

                    match asm with
                    | null ->
                        None
                    | t ->
                        Some t.Module.Assembly.Name.Name
                DisplayName = noGen t.FSharpName
                FullName = name
            }

        let rec parseType (t: TypeReference) =
            if t.IsGenericParameter then
                let t = t :?> GenericParameter
                match t.Owner with
                | :? TypeReference ->
                    CM.ClassParameterType {
                        Name = t.Name
                        Position = t.Position
                    }
                | _ ->
                    CM.MethodParameterType {
                        Name = t.Name
                        Position = t.Position
                    }
            elif t.IsGenericInstance then
                let t = t :?> GenericInstanceType
                let ps = [for p in t.GenericArguments -> parseType p]
                if t.Namespace = "System" && t.Name.StartsWith "Tuple" then
                    CM.TupleType ps
                elif t.Namespace = "Microsoft.FSharp.Core"
                     && t.Name = "FSharpFunc`2" then
                    CM.FunctionType (ps.[0], ps.[1])
                else
                    CM.GenericType(parseTR t, ps)
            elif t.IsArray then
                let t = t :?> ArrayType
                CM.ArrayType(parseType t.ElementType, t.Rank)
            elif t.IsPointer then
                let t = t :?> PointerType
                CM.PointerType (parseType t.ElementType)
            elif t.IsByReference then
                let t = t :?> ByReferenceType
                CM.ByReferenceType (parseType t.ElementType)
            elif t.Module.TypeSystem.Void = t then
                CM.UnitType
            else
                CM.ConcreteType (parseTR t)

        let parseParams (ps: seq<ParameterDefinition>) =
            ps
            |> Seq.map (fun p ->
                let parameterType = 
                    if p.IsOptionalParameter  
                    then 
                        let git = p.ParameterType :?> GenericInstanceType
                        CM.OptionalParameterType (parseType git.GenericArguments.[0])
                    else 
                        parseType p.ParameterType
                {
                    Name = p.Name
                    Type = parameterType
                } : CM.Parameter)
            |> Seq.toList

        let parseMD (m: MemberReference) =
            let id =
                match m with
                | :? TypeReference as x -> XmlDoc.Id.Type x
                | :? MethodReference as x -> XmlDoc.Id.Method x
                | :? PropertyReference as x -> XmlDoc.Id.Property x
                | :? FieldReference as x -> XmlDoc.Id.Field x
                | _ -> invalidArg "m" "MemberReference not supported."
            {
                Documentation = documentation.Lookup id
                Id = id
            }

        let parseProperty (p: PropertyDefinition) =
            {
                Record.Default<CM.Property> with
                    Name = p.FSharpName
                    Type = parseType p.PropertyType
                    IsStatic = p.IsStatic
                    CanRead = p.GetMethod <> null && p.GetMethod.IsPublic
                    CanWrite = p.SetMethod <> null && p.SetMethod.IsPublic
                    Parameters = parseParams p.Parameters
            }
            |> Record.Patch (parseMD p)

        let parseModuleProperty (p: PropertyDefinition) =
            {
                Record.Default<CM.ModuleProperty> with
                    Name = p.FSharpName
                    Type = parseType p.PropertyType
                    IsMutable = p.SetMethod <> null && p.SetMethod.IsPublic
                    GenericParameters = []
            }
            |> Record.Patch (parseMD p)

        let parseConstructor (c: MethodDefinition) =
            {
                Record.Default<CM.Constructor> with
                    Parameters = parseParams c.Parameters
            }
            |> Record.Patch (parseMD c)

        let parseGeneric (g: GenericParameter) : CM.GenericParameter =
            {
                Name = g.Name
                Position = g.Position
            }

        let parseGenerics (g: seq<GenericParameter>) =
            [for x in g -> parseGeneric x]

        let parseModuleMethod (m: MethodDefinition) =
            let parameters =
                let p =  parseParams m.Parameters
                match m.TryGetCompilationArgumentCounts() with
                | Some counts ->
                    let queue = new System.Collections.Generic.Queue<_>(p)
                    let converted = ResizeArray<_>()
                    for c in counts do
                        match c with
                        | 0 -> ()
                        | 1 -> converted.Add(queue.Dequeue())
                        | c ->
                            let args = [for _ in 1..c -> queue.Dequeue().Type]
                            let paramType = CM.TupleType args
                            let tupledParam = 
                                {
                                    Type = paramType
                                    Name = ""
                                } : CM.Parameter
                            converted.Add(tupledParam)
                    List.ofSeq converted

                | None ->
                    match p with
                    | [] | [_] -> p
                    | _ -> [({Name = ""; Type = CM.TupleType [for pp in p -> pp.Type]} : CM.Parameter)] 
                
            let name = 
                let fsName = m.FSharpName
                if m.IsOperator 
                then 
                    match SymbolicOperators.TryGet m.Name with
                    | Some n -> "(" + n + ")"
                    | None -> fsName
                else fsName
            {
                Record.Default<CM.ModuleMethod> with
                    Name = name
                    GenericParameters = parseGenerics m.GenericParameters
                    ReturnType = parseType m.ReturnType
                    Parameters = parameters
            }
            |> Record.Patch (parseMD m)

        let parseMethod (m: MethodDefinition) =
            let r =
                Record.Default<CM.Method>
                |> Record.Patch (parseModuleMethod m) 
            { r with IsStatic = m.IsStatic }

        let parseField (f: FieldDefinition) =
            {
                Record.Default<CM.Field> with
                    Name = f.FSharpName
                    Type = parseType f.FieldType
                    IsStatic = f.IsStatic
            }
            |> Record.Patch (parseMD f)

        let getMethods (def: TypeDefinition) =
            def.Methods
            |> Seq.filter (fun m ->
                m.IsPublic && not (
                    m.IsConstructor
                    || m.IsGetter
                    || m.IsSetter
                    || m.IsCompilerGenerated
                    || m.IsUnionCase
                ))
            |> Seq.toList

        let getConstructors (def: TypeDefinition) =
            def.Methods
            |> Seq.filter (fun m ->
                m.IsPublic && m.IsConstructor && not m.IsCompilerGenerated)
            |> Seq.toList

        let getProperties (def: TypeDefinition) =
            def.Properties
            |> Seq.filter (fun p ->
                p.IsPublic
                && not p.IsCompilerGenerated
                && not p.IsField)
            |> Seq.toList

        let getFields (def: TypeDefinition) =
            def.Fields
            |> Seq.filter (fun f ->
                f.IsPublic
                && not f.IsCompilerGenerated)
            |> Seq.toList

        let parseInterface (def: TypeDefinition) =
            let methods = List.map parseMethod (getMethods def)
            let properties = List.map parseProperty (getProperties def)
            let interfaces =
                def.Interfaces
                |> Seq.map parseType
                |> Seq.toList
            {
                Record.Default<CM.Interface> with
                    Name = def.FSharpName
                    GenericParameters = parseGenerics def.GenericParameters
                    Interfaces = interfaces
                    Methods = methods
                    Properties = properties
            }
            |> Record.Patch (parseMD def)

        let parseRecordField (def: PropertyDefinition) =
            {
                Record.Default<CM.RecordField> with
                    Name = def.FSharpName
                    Type = parseType def.PropertyType
            }
            |> Record.Patch (parseMD def)

        let parseRecord (def: TypeDefinition) =
            let r =
                Record.Default<CM.Record>
                |> Record.Patch (parseInterface def)
            { r with
                Fields =
                    def.Properties
                    |> Seq.filter (fun x -> x.IsPublic && x.IsField)
                    |> Seq.map parseRecordField
                    |> Seq.toList }

        let parseUnionCase (def: TypeDefinition) =
            { Record.Default<CM.UnionCase> with
                Name = def.UnionCaseName
                Fields =
                    def.Fields
                    |> Seq.map (fun f ->
                        parseType f.FieldType)
                    |> Seq.toList
            }
            |> Record.Patch (parseMD def)

        let parseUnion (def: TypeDefinition) =
            let r =
                Record.Default<CM.Union>
                |> Record.Patch (parseInterface def)
            
            let makeUnionCase name fields = 
                { Record.Default<CM.UnionCase> with Name = name; Fields = fields } 

            let makeNoArgsCase name = 
                makeUnionCase name []

            let makeCaseForFactory name (m : MethodDefinition) = 
                let fieldTypes = 
                    m.Parameters
                    |> Seq.map (fun p -> parseType p.ParameterType)
                    |> Seq.toList
                makeUnionCase name fieldTypes

            let findUnionCaseFactory tag = 
                def.Methods
                |> Seq.find(fun m -> m.IsUnionCase && m.GetUnionCaseTag() = tag)

            let map = 
                def.NestedTypes
                |> Seq.filter(fun nt -> nt.IsNestedPublic)
                |> Seq.map (fun nt -> nt.Name, nt)
                |> dict

            let parseCasesFromTagFields fields = 
                fields
                |> Seq.map (fun (tag, f : FieldDefinition) ->
                    match map.TryGetValue f.Name with
                    | true, t -> 
                        parseUnionCase t 
                    | false, _ -> 
                        let factory = findUnionCaseFactory tag
                        if factory.Parameters.Count = 0
                        then makeNoArgsCase f.Name
                        else makeCaseForFactory f.Name factory
                    )
                |> Seq.toList
            
            match map.TryGetValue "Tags" with
            | true, tagsType ->
                let fields = 
                    tagsType.Fields 
                    |> Seq.filter (fun f -> f.IsLiteral)
                    |> Seq.map(fun f -> unbox<int> f.Constant, f)
                    |> Seq.sortBy fst
                    |> List.ofSeq

                { r with Cases = parseCasesFromTagFields fields }
            | false, _ ->
                let unionCases = 
                    def.Methods
                    |> Seq.filter (fun m -> m.IsUnionCase)
                    |> Seq.toList
                match unionCases with
                | [c] -> {r with Cases = [makeCaseForFactory def.Name c] }
                | _ -> failwithf "Invalid structure of the discriminated union %s" def.Name

        let parseClass (def: TypeDefinition) =
            let r =
                Record.Default<CM.Class>
                |> Record.Patch (parseInterface def)
            { r with
                BaseType = parseType def.BaseType
                Fields = List.map parseField (getFields def)
                Constructors = List.map parseConstructor (getConstructors def)
            }

        let rec parseModule (def: TypeDefinition) =
            {
                Record.Default<CM.Module> with
                    Name = def.FSharpName
                    Members =
                        Seq.concat [
                            def.Methods
                            |> Seq.filter (fun x -> x.IsPublic)
                            |> Seq.map (CM.MethodMember <<
                                parseModuleMethod)

                            def.Properties
                            |> Seq.filter (fun x -> x.IsPublic)
                            |> Seq.map (CM.PropertyMember <<
                                parseModuleProperty)

                            def.NestedTypes
                            |> Seq.filter (fun x -> x.IsNestedPublic)
                            |> Seq.map (fun x ->
                                if x.IsModule then
                                    CM.ModuleMember (parseModule x)
                                elif x.IsUnion then
                                    CM.UnionMember (parseUnion x)
                                elif x.IsRecord then
                                    CM.RecordMember (parseRecord x)
                                elif x.IsInterface then
                                    CM.InterfaceMember (parseInterface x)
                                else
                                    CM.ClassMember (parseClass x))
                        ]
                        |> Seq.toList
            }
            |> Record.Patch (parseMD def)

        let parseAssembly (a: AssemblyDefinition) : CM.Assembly =
            let ns = Dictionary()
            for t in a.MainModule.Types do
                if t.IsPublic then
                    let r =
                        match ns.TryGetValue t.Namespace with
                        | true, ns -> ns
                        | _ ->
                            let r : CM.Namespace =
                                {
                                    Name = t.Namespace
                                    Unions = []
                                    Records = []
                                    Classes = []
                                    Interfaces = []
                                    Modules = []
                                }
                            ns.[t.Namespace] <- r
                            r
                    ns.[t.Namespace] <-
                        if t.IsInterface then
                            { r with
                                Interfaces = parseInterface t :: r.Interfaces }
                        elif t.IsModule then
                            { r with Modules = parseModule t :: r.Modules }
                        elif t.IsRecord then
                            { r with Records = parseRecord t :: r.Records }
                        elif t.IsUnion then
                            { r with Unions = parseUnion t :: r.Unions }
                        else
                            { r with Classes = parseClass t :: r.Classes }
            {
                Name = a.Name.Name
                Version = a.Name.Version
                Namespaces =
                    ns.Values
                    |> Seq.sortBy (fun x -> x.Name)
                    |> Seq.toList
            }

        parseAssembly assembly

    /// Loads an assembly from a given path.
    let LoadFile (path: string) searchPaths =
        
        let a = AssemblyDefinition.ReadAssembly path

        let d =
            let p = Path.ChangeExtension(path, ".xml")
            if File.Exists p then
                XmlDoc.Document.FromFile p
            else
                XmlDoc.Document.Empty
        ParseAssembly a d
