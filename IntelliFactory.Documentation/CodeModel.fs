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

/// Provides types to represents the documented public API of assemblies.
module CodeModel =
    open System
    open System.Collections.Generic
    

    /// Represents documentation parsed from XML comments.
    type Documentation = XmlDoc.Node

    /// Represents a type reference.
    type TypeReference =
        {
            /// The declaring assembly.
            Assembly : option<string>

            /// The fully qualified name.
            FullName : string

            /// The short name without generics annotations.
            DisplayName : string
        }

        /// The XML documentation ID.
        member this.Id =
            XmlDoc.Id.Raw("T:" + this.FullName)

    /// Represents generic parameters.
    type GenericParameter =
        {
            Name : string
            Position : int
        }

    /// Represents types of values and parameters.
    type Type =
        | InferredType
        | ConcreteType of TypeReference
        | ArrayType of Type * int
        | PointerType of Type
        | ByReferenceType of Type
        | GenericType of TypeReference * list<Type>
        | ClassParameterType of GenericParameter
        | MethodParameterType of GenericParameter
        | TupleType of list<Type>
        | FunctionType of Type * Type
        | OptionalParameterType of Type
        | UnitType

        /// Pretty-prints the type into a sequence of tokens.
        member this.Tokens =
            let q = Queue()
            let name x = q.Enqueue (ReferenceToken x)
            let text x = q.Enqueue (TextToken x)
            let rec tokenize (t: Type) =
                match t with
                | ConcreteType n ->
                    match TypeAbbreviations.Abbreviate n.FullName with
                    | Some x -> name { n with DisplayName = x }
                    | None -> name n
                | GenericType (d, args) ->
                    tokenize (ConcreteType d)
                    text "<"
                    tokenize args.Head
                    for a in args.Tail do
                        text ","
                        tokenize a
                    text ">"
                | FunctionType (d, r) ->
                    tokenizeWithFunctionCheck d
                    text " → "
                    tokenize r
                | TupleType xs ->
                    tokenizeWithFunctionCheck xs.[0]
                    for x in xs.Tail do
                        text " × "
                        tokenizeWithFunctionCheck x
                | ArrayType (t, r) ->
                    tokenize t
                    text "["
                    text (String.init (r - 1) (fun _ -> ","))
                    text "]"
                | ByReferenceType t ->
                    text "byref<"
                    tokenize t
                    text ">"
                | ClassParameterType x ->
                    text "'"
                    text x.Name
                | MethodParameterType x ->
                    text "'"
                    text x.Name
                | PointerType t ->
                    text "ptr<"
                    tokenize t
                    text ">"
                | InferredType ->
                    text "_"
                | UnitType ->
                    text "unit"
                | OptionalParameterType t ->
                    text "?"
                    tokenizeWithFunctionCheck t
            
            and tokenizeWithFunctionCheck = function
                | (FunctionType _) as f ->
                    text "(" 
                    tokenize f
                    text ")"
                | t -> tokenize t

            tokenize this
            q :> seq<_>

    /// Represents serialized type tokens.
    and TypeToken =
        | TextToken of string
        | ReferenceToken of TypeReference
    

    /// An interface for type members such as methods and properties.
    type IMember =
        abstract member Id : XmlDoc.Id
        abstract member Name : string
        abstract member Type : Type
        abstract member Documentation : option<Documentation>
        abstract member IsStatic : bool

    module Members = 
        let sort (members : #seq<#IMember>) = 
            members
            |> Seq.map(fun m -> m :> IMember)
            |> Seq.sortBy (fun m -> m.Name)
            |> Seq.toList

    /// Represents method parameters.
    type Parameter =
        {
            Name : string
            Type : Type
        }

    /// Represents fields.
    type Field =
        {
            Name : string
            Type : Type
            IsStatic : bool
            Id : XmlDoc.Id
            Documentation : option<Documentation>
        }

        interface IMember with
            member this.Id = this.Id
            member this.Name = this.Name
            member this.Type = this.Type
            member this.IsStatic = this.IsStatic
            member this.Documentation = this.Documentation

    /// Represents methods.
    type Method =
        {
            Name : string
            GenericParameters : list<GenericParameter>
            Parameters : list<Parameter>
            ReturnType : Type
            IsStatic : bool
            Id : XmlDoc.Id
            Documentation : option<Documentation>
        }

        interface IMember with
            member this.Id = this.Id
            member this.Name = this.Name
            member this.Type =
                (this.Parameters, this.ReturnType)
                ||> List.foldBack (fun p rT ->
                    FunctionType (p.Type, rT))
            member this.IsStatic = this.IsStatic
            member this.Documentation = this.Documentation

    /// Represents constructors.
    type Constructor =
        {
            Parameters : list<Parameter>
            Id : XmlDoc.Id
            Documentation : option<Documentation>
        }

        interface IMember with
            member this.Id = this.Id
            member this.Name = "new"
            member this.Type =
                let d =
                    match this.Parameters with
                    | []    -> UnitType
                    | [x]   -> x.Type
                    | xs    -> TupleType [for x in xs -> x.Type]
                FunctionType (d, InferredType)
            member this.IsStatic = true
            member this.Documentation = this.Documentation

    /// Represents properties.
    type Property =
        {
            Name : string
            Type : Type
            IsStatic : bool
            CanWrite : bool
            CanRead : bool
            Parameters : list<Parameter>
            Id : XmlDoc.Id
            Documentation : option<Documentation>
        }

        interface IMember with
            member this.Id = this.Id
            member this.Name = this.Name
            member this.Type = this.Type
            member this.IsStatic = this.IsStatic
            member this.Documentation = this.Documentation

    /// Represents union cases.
    type UnionCase =
        {
            Name : string
            Fields : list<Type>
            Id : XmlDoc.Id
            Documentation : option<Documentation>
        }

    /// Represents record fields.
    type RecordField =
        {
            Name : string
            Type : Type
            IsMutable : bool
            Id : XmlDoc.Id
            Documentation : option<Documentation>
        }

        interface IMember with
            member this.Id = this.Id
            member this.Name = this.Name
            member this.Type = this.Type
            member this.IsStatic = false
            member this.Documentation = this.Documentation

    type TypeKind = 
        | Union = 0
        | Record = 1
        | Class  = 2
        | Module = 3
        | Interface = 4

    /// Generalizes over all data types and modules.
    type IType =
        abstract member Id : XmlDoc.Id
        abstract member Documentation : option<Documentation>
        abstract member BaseType : option<Type>
        abstract member Name : string
        abstract member Members : list<IMember>
        abstract member RecordFields : list<RecordField>
        abstract member UnionCases : list<UnionCase>
        abstract member Interfaces : list<Type>
        abstract member NestedTypes : list<IType>

        abstract member Properties : list<IMember>
        abstract member Methods : list<IMember>
        abstract member Kind : TypeKind

    /// Represents union types.
    type Union =
        {
            Name : string
            GenericParameters : list<GenericParameter>
            Interfaces : list<Type>
            Cases : list<UnionCase>
            Properties : list<Property>
            Methods : list<Method>
            Id : XmlDoc.Id
            Documentation : option<Documentation>
        }

        interface IType with
            member this.Id = this.Id
            member this.BaseType = None
            member this.Name = this.Name
            member this.RecordFields = []
            member this.UnionCases = this.Cases
            member this.Interfaces = this.Interfaces
            member this.Members =
                seq {
                    for p in this.Properties do
                        yield p :> IMember
                    for m in this.Methods do
                        yield m :> IMember
                } 
                |> Members.sort 
            member this.Properties = (this : Union).Properties |> Members.sort
            member this.Methods = []
            member this.NestedTypes = []
            member this.Documentation = this.Documentation
            member this.Kind = TypeKind.Union

    /// Represents record types.
    type Record =
        {
            Name : string
            GenericParameters : list<GenericParameter>
            Interfaces : list<Type>
            Fields : list<RecordField>
            Properties : list<Property>
            Methods : list<Method>
            Id : XmlDoc.Id
            Documentation : option<Documentation>
        }

        interface IType with
            member this.Id = this.Id
            member this.BaseType = None
            member this.Name = this.Name
            member this.RecordFields = this.Fields
            member this.UnionCases = []
            member this.Interfaces = this.Interfaces
            member this.Members =
                seq {
                    for p in this.Properties do
                        yield p :> IMember
                    for m in this.Methods do
                        yield m :> IMember
                }
                |> Members.sort
            member this.Properties = (this : Record).Properties |> Members.sort
            member this.Methods = (this : Record).Methods |> Members.sort
            
            member this.NestedTypes = []
            member this.Documentation = this.Documentation
            member this.Kind = TypeKind.Record

    /// Represents module let-bound properties.
    type ModuleProperty =
        {
            Name : string
            GenericParameters : list<GenericParameter>
            Type : Type
            IsMutable : bool
            Id : XmlDoc.Id
            Documentation : option<Documentation>
        }

        interface IMember with
            member this.Id = this.Id
            member this.Name = this.Name
            member this.Type = this.Type
            member this.IsStatic = true
            member this.Documentation = this.Documentation

    /// Represents module let-bound methods.
    type ModuleMethod =
        {
            Name : string
            GenericParameters : list<GenericParameter>
            Parameters : list<Parameter>
            ReturnType : Type
            Id : XmlDoc.Id
            Documentation : option<Documentation>
        }

        interface IMember with
            member this.Id = this.Id
            member this.Name = this.Name
            member this.Type =
                (this.Parameters, this.ReturnType)
                ||> List.foldBack (fun p rT ->
                    FunctionType (p.Type, rT))
            member this.IsStatic = true
            member this.Documentation = this.Documentation

    /// Represents classes.
    type Class =
        {
            Name : string
            GenericParameters : list<GenericParameter>
            BaseType : Type
            Interfaces : list<Type>
            Constructors : list<Constructor>
            Fields : list<Field>
            Methods : list<Method>
            Properties : list<Property>
            Id : XmlDoc.Id
            Documentation : option<Documentation>
        }

        interface IType with
            member this.Id = this.Id
            member this.BaseType = None
            member this.Name = this.Name
            member this.RecordFields = []
            member this.UnionCases = []
            member this.Interfaces = this.Interfaces
            member this.Members =
                seq {
                    for c in this.Constructors do
                        yield c :> IMember
                    for f in this.Fields do
                        yield f :> IMember
                    for p in this.Properties do
                        yield p :> IMember
                    for m in this.Methods do
                        yield m :> IMember
                }
                |> Seq.sortBy (fun x ->
                    match x with
                    | :? Constructor -> (0, string x.Id)
                    | _ -> ((if x.IsStatic then 1 else 2), x.Name))
                |> Seq.toList
            member this.Properties = (this : Class).Properties |> Members.sort
            member this.Methods = (this : Class).Methods |> Members.sort
            member this.NestedTypes = []
            member this.Documentation = this.Documentation
            member this.Kind = TypeKind.Class

    /// Represents interfaces.
    type Interface =
        {
            Name : string
            GenericParameters : list<GenericParameter>
            Interfaces : list<Type>
            Methods : list<Method>
            Properties : list<Property>
            Id : XmlDoc.Id
            Documentation : option<Documentation>
        }

        interface IType with
            member this.Id = this.Id
            member this.BaseType = None
            member this.Name = this.Name
            member this.RecordFields = []
            member this.UnionCases = []
            member this.Interfaces = this.Interfaces
            member this.Members =
                seq {
                    for p in this.Properties do
                        yield p :> IMember
                    for m in this.Methods do
                        yield m :> IMember
                }
                |> Members.sort
            member this.Properties = (this : Interface).Properties |> Members.sort
            member this.Methods = (this : Interface).Methods |> Members.sort
            member this.NestedTypes = []
            member this.Documentation = this.Documentation
            member this.Kind = TypeKind.Interface

    /// Represents F# modules.
    type Module =
        {
            Name : string
            Members : list<ModuleMember>
            Id : XmlDoc.Id
            Documentation : option<Documentation>
        }

        interface IType with
            member this.Properties = 
                this.Members 
                |> Seq.choose(function PropertyMember x -> Some (x :> IMember) | _ -> None) 
                |> Seq.toList
            member this.Methods = 
                this.Members 
                |> Seq.choose(function MethodMember x -> Some (x :> IMember) | _ -> None) 
                |> Seq.toList

            member this.Id = this.Id
            member this.BaseType = None
            member this.Name = this.Name
            member this.RecordFields = []
            member this.UnionCases = []
            member this.Interfaces = []
            member this.Members =
                seq {
                    for m in this.Members do
                        match m with
                        | PropertyMember x ->
                            yield x :> IMember
                        | MethodMember x ->
                            yield x :> IMember
                        | _ ->
                            ()
                }
                |> Members.sort
            member this.NestedTypes =
                seq {
                    for x in this.Members do
                        match x with
                        | RecordMember x ->
                            yield x :> IType
                        | UnionMember x ->
                            yield x :> IType
                        | InterfaceMember x ->
                            yield x :> IType
                        | ClassMember x ->
                            yield x :> IType
                        | ModuleMember x ->
                            yield x :> IType
                        | _ ->
                            ()
                }
                |> Seq.sortBy (fun x ->
                    match x with
                    | :? Interface -> (0, x.Name)
                    | _ -> (1, x.Name))
                |> Seq.toList
            member this.Documentation = this.Documentation
            member this.Kind = TypeKind.Module

    /// Unifies F# module members.
    and ModuleMember =
        | MethodMember of ModuleMethod
        | PropertyMember of ModuleProperty
        | RecordMember of Record
        | UnionMember of Union
        | InterfaceMember of Interface
        | ClassMember of Class
        | ModuleMember of Module

    /// Represents namespaces.
    type Namespace =
        {
            Name : string
            Unions : list<Union>
            Records : list<Record>
            Classes : list<Class>
            Modules : list<Module>
            Interfaces : list<Interface>
        }

        member this.Types =
            [
                for x in this.Interfaces do
                    yield x :> IType
                yield!
                    seq {
                        for x in this.Unions do
                            yield x :> IType
                        for x in this.Records do
                            yield x :> IType
                        for x in this.Modules do
                            yield x :> IType
                        for x in this.Classes do
                            yield x :> IType
                    }
                    |> Seq.sortBy (fun x -> x.Name)
                    |> Seq.toList
            ]

    /// Represents assemblies.
    type Assembly =
        {
            Name : string
            Namespaces : list<Namespace>
            Version : Version
        }

    /// Represents a collection of documented assemblies.
    type AssemblySet(assemblies : list<Assembly>) =
        let rec list (t : IType) = seq {
            yield t
            for nt in t.NestedTypes do
                yield! list nt 
            }
        let types = 
            let l = 
                assemblies
                    |> Seq.collect (fun a -> a.Namespaces)
                    |> Seq.collect (fun ns -> ns.Types)
                    |> Seq.collect list
            System.Collections.Generic.HashSet(l)
            
        let typeIds = System.Collections.Generic.HashSet(types |> Seq.map (fun x -> x.Id))

        member this.Assemblies = assemblies
        member this.AllTypes = types
        member this.AllTypeIds = typeIds

    type TypeInfo =
        {
            Type : IType
            Namespace : Namespace
            Assembly : Assembly
        } 