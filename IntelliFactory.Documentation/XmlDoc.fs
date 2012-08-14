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

/// Matches Mono.Cecil assembly definitions to XML documentation identifiers.
module XmlDoc =
    open System.Collections.Generic
    open System.IO
    open System.Web
    open System.Xml
    open System.Xml.Linq
    open Mono.Cecil


    module private IdNormalizer = 
        type private Buf = System.Text.StringBuilder

        type private TypeCon = 
            {
                Name : string
                TypeParams : TypeCon list
                Array : string option
            }
            with
            override this.ToString() =
                let p = 
                    this.TypeParams 
                    |> Seq.map string 
                    |> String.concat ","

                let hasTypeParams = not (List.isEmpty this.TypeParams)
                let hasTick = this.Name.IndexOf('`') <> -1
                let array = 
                    match this.Array with
                    | Some s -> s
                    | _ -> ""

                match hasTypeParams, hasTick with
                | true, true -> sprintf "%s{%s}%s" this.Name p array
                | true, false -> sprintf "%s`%d{%s}%s" this.Name (List.length this.TypeParams) p array
                | _ -> sprintf "%s%s" this.Name array
    
        type private MethodXmlId = 
            {
                Name : string
                Params : TypeCon list 
            }
            with
            override this.ToString() = 
                let p = 
                    this.Params 
                    |> Seq.map string 
                    |> String.concat ","
                sprintf "%s(%s)" this.Name p

        exception XmlDocIdProcessError of string

        let parse (s : string) = 

            let rec parseTypeConstructor pos : TypeCon * int =
                let rec loop p (buf : Buf) =
                    if p >= s.Length then raise (XmlDocIdProcessError "Invalid xml-doc id")
                
                    match s.[p] with
                    | ')' 
                    | '}' 
                    | ',' -> {Name = buf.ToString(); TypeParams = []; Array = None}, p
                    | '{' ->
                        let name = buf.ToString()
                        let tp, next = parseTypeConstructors (p + 1) []
                        if (s.[next] <> '}') then raise (XmlDocIdProcessError "mismatched brackets position")
                        let arr, next =
                            if next < s.Length - 1 && s.[next + 1] = '['
                            then 
                                let arr, next = parseArray (next + 1)
                                Some arr, next
                            else None, next
                        {Name = name; TypeParams = tp; Array = arr}, next + 1
                    | x -> loop (p + 1) (buf.Append x) 
                loop pos (Buf())

            and parseTypeConstructors pos acc = 
                let tyCon, next = parseTypeConstructor pos
                let acc = tyCon::acc            
                if s.[next] = ',' then parseTypeConstructors (next + 1) acc
                else (List.rev acc), next

            and parseArray pos = 
                let rec loop p (buf : Buf) = 
                    if p >= s.Length then raise (XmlDocIdProcessError "Invalid xml-doc id")
                    match s.[p] with
                    | ']' ->
                        buf.Append (']') |> ignore
                        if p < s.Length - 1 && s.[p + 1] = '['
                        then loop (p + 1) buf
                        else (buf.ToString()), p
                    | x -> loop (p + 1) (buf.Append x)

                loop pos (Buf())

            if s.StartsWith "M:" 
            then 
                let idx = s.IndexOf '('
                if idx < 0 then s
                else
                    let name = s.Substring(2, idx - 2)
                    let p, _ = parseTypeConstructors (idx + 1) []
                    let id = {Name = name; Params = p}
                    "M:" + id.ToString()
            else s

        

    module private IdWriter =

        let Escape (name: string) =
            name.Replace('.', '#')

        let WriteType (tR: TypeReference) (out: TextWriter) =
            let rec loop (tR: TypeReference) =
                match tR.DeclaringType with
                | null -> out.Write tR.Namespace
                | dT   -> loop dT
                out.Write '.'
                out.Write (Escape tR.Name)
            loop tR

        let WriteTypeSpec (tR: GenericInstanceType) (out: TextWriter) =
            let rec loop (tR: TypeReference) =
                match tR.DeclaringType with
                | null -> out.Write tR.Namespace
                | dT   -> loop dT
                out.Write '.'
                let name = Escape tR.Name
                out.Write name
            loop tR

        let rec WriteParameter (pD: ParameterReference) (out: TextWriter) =
            WriteParameterType pD.ParameterType out

        and WriteParameterType (pT: TypeReference) (out: TextWriter) =
            if pT.IsGenericParameter then
                let pT = pT :?> GenericParameter
                match pT.Owner with
                | :? TypeReference  -> "`"
                | _  -> "``"
                |> out.Write
                out.Write pT.Position
            elif pT.IsGenericInstance then
                let gi = pT :?> GenericInstanceType
                WriteTypeSpec gi out
                out.Write '{'
                WriteParameterType gi.GenericArguments.[0] out
                for p in Seq.skip 1 gi.GenericArguments do
                    out.Write ','
                    WriteParameterType p out
                out.Write '}'
            elif pT.HasGenericParameters then
                WriteType pT out
                out.Write '{'
                WriteParameterType pT.GenericParameters.[0] out
                for p in Seq.skip 1 pT.GenericParameters do
                    out.Write ','
                    WriteParameterType p out
                out.Write '}'
            elif pT.IsArray then
                let pT = pT :?> ArrayType
                WriteParameterType pT.ElementType out
                if pT.Rank = 1 then
                    out.Write "[]"
                else
                    out.Write "[0:"
                    for n = 2 to pT.Rank do
                        out.Write ",0:"
                    out.Write ']'
            elif pT.IsPointer then
                let pT = pT :?> PointerType
                WriteParameterType pT.ElementType out
                out.Write '*'
            elif pT.IsByReference then
                let pT = pT :?> ByReferenceType
                WriteParameterType pT.ElementType out
                out.Write '@'
            else
                WriteType pT out

        let WriteParameters (ps: seq<#ParameterReference>) (out: TextWriter) =
            if not (Seq.isEmpty ps) then
                out.Write '('
                WriteParameter (Seq.head ps) out
                for p in Seq.skip 1 ps do
                    out.Write ','
                    WriteParameter p out
                out.Write ')'

    /// Represents XML documentation identifiers.
    type Id = private { id : string } with

        /// Parses back the fully qualified name of the identifier.
        member this.FullName =
            let n =
                if this.id.Length > 2 && this.id.[2] = ':' then
                    this.id.Substring 2
                else
                    this.id
            match n.LastIndexOfAny([| '('; '`' |]) with
            | -1 -> n
            | k  -> n.Substring(0, k)

        /// Parses back the short name of the identifier.
        member this.Name =
            let n = this.FullName
            match n.LastIndexOfAny([| '.' |]) with
            | -1 -> n
            | k  -> n.Substring (k + 1)

        override this.ToString() = this.id

        /// Gets the namespace XML documentation ID.
        static member Namespace ns =
            { id = "N:" + ns }

        /// Constructs a custom ID.
        static member Raw(raw: string) =
            { id = raw }

        /// Gets the type XML documentation ID.
        static member Type (tR: TypeReference) =
            use out = new StringWriter()
            out.Write "T:"
            IdWriter.WriteType tR out
            { id = out.ToString() }

        /// Gets the field XML documentation ID.
        static member Field (fD: FieldReference) =
            use out = new StringWriter()
            out.Write "F:"
            IdWriter.WriteType fD.DeclaringType out
            out.Write '.'
            out.Write (IdWriter.Escape fD.Name)
            { id = out.ToString() }

        /// Gets the method XML documentation ID.
        static member Method (mD: MethodReference) =
            use out = new StringWriter()
            out.Write "M:"
            IdWriter.WriteType mD.DeclaringType out
            out.Write '.'
            out.Write (IdWriter.Escape mD.Name)
            if mD.HasGenericParameters then
                out.Write "``"
                out.Write mD.GenericParameters.Count
            IdWriter.WriteParameters mD.Parameters out
            { id = out.ToString() }

        /// Gets the property XML documentation ID.
        static member Property (pD: PropertyReference) =
            use out = new StringWriter()
            out.Write "P:"
            IdWriter.WriteType pD.DeclaringType out
            out.Write '.'
            out.Write (IdWriter.Escape pD.Name)
            IdWriter.WriteParameters pD.Parameters out
            { id = out.ToString() }

    /// Represents HTML.
    type Html = string

    /// Represents parsed XML documentation node.
    type Node =
        {
            IsUserDoc : bool
            Summary : Html
            Remarks : option<Html>
            Parameters : IDictionary<string,Html>
            OrderedParameters : string list 
            TypeParameters : IDictionary<string,Html>
            Returns : option<Html>
            Exceptions : IDictionary<Id,Html>
            SeeAlso : list<Id>
        }

        /// Constructs a simple Documentation value based on a summary.
        static member Default(summary: string) =
            {
                IsUserDoc = false
                Summary = HttpUtility.HtmlEncode summary
                Remarks = None
                Parameters = Dictionary()
                OrderedParameters = []
                TypeParameters = Dictionary()
                Returns = None
                Exceptions = Dictionary()
                SeeAlso = []
            }

        /// Parses XML documentation from a raw XML string.
        static member Parse(rawXml: string) =
            try
                let (!) x = XName.Get x
                let doc = XDocument.Parse ("<xml>" + rawXml + "</xml>")
                let find name =
                    doc.Descendants(XName.Get name)
                    |> Seq.toList
                //BT This read function also writes. Destructive.  Prevents us from reading from the same XML for different record props :(.
                let read (x: XElement) : Html =
                    let n = x.Name.LocalName
                    x.Name <- !"div"
                    x.SetAttributeValue(!"class", "if-doc-" + n)
                    for e in find "c" do
                        e.Name <- !"code"
                    for e in find "para" do
                        e.Name <- !"p"
                    for e in find "see" do
                        let attr = e.Attribute(!"cref")
                        let value = "#" + View.IdEncode attr.Value
                        attr.Remove()
                        e.Name <- !"a"
                        e.SetAttributeValue(!"href", value)
                        e.SetAttributeValue(!"class", "if-doc-link")
                    x.ToString()
                {
                    IsUserDoc = 
                        match find "user" with
                        | [] -> false 
                        | x :: _ -> true 
                    Summary =
                        match find "summary" with
                        | [] -> HttpUtility.HtmlEncode rawXml
                        | x :: _ -> read x
                    Remarks =
                        match find "remarks" with
                        | [] -> None
                        | x :: _ -> Some (read x)
                    Parameters =
                        let d = Dictionary()
                        for p in find "param" do
                            let n = p.Attribute(!"name").Value
                            d.[n] <- read p
                        d
                    TypeParameters =
                        let d = Dictionary()
                        for p in find "typeparam" do
                            let n = p.Attribute(!"name").Value
                            d.[n] <- read p
                        d
                    Returns =
                        match find "returns" with
                        | [] -> None
                        | x :: _ -> Some (read x)
                    Exceptions =
                        let d = Dictionary()
                        for p in find "exception" do
                            let attr = p.Attribute(!"cref")
                            let attr = if attr = null then p.Attribute(!"href") else attr
                            if attr <> null 
                            then
                                let n = attr.Value
                                d.[Id.Raw n] <- read p
                        d
                    SeeAlso =
                        let q = Queue()
                        for p in find "seealso" do
                            let r = p.Attribute(!"cref").Value
                            q.Enqueue(Id.Raw r)
                        Seq.toList q
                }
            with _ ->
                Node.Default rawXml

    /// Represents a parsed XML documentation file.
    type Document = private { data: Dictionary<string,Node> } with

        /// Creates an empty document.
        static member Empty =
            { data = Dictionary() }

        /// Parses XML documentation from a reader.
        static member Parse(reader: XmlReader) =
            let d = Dictionary()
            let xml = reader
            if xml.ReadToDescendant "members" then
                xml.MoveToContent() |> ignore
                while xml.ReadToFollowing "member" do
                    let name = xml.GetAttribute "name"
                    let doc = xml.ReadInnerXml()
                    if name <> "" && name <> null then
                        if name.Contains "Unzip3" then
                            printfn "1"
                        let n = IdNormalizer.parse name
                        d.[n] <- Node.Parse doc
            { data = d }

        /// Parses a given XML documentation file.
        static member FromFile (path: string) =
            use xml = XmlReader.Create (File.OpenText path)
            Document.Parse xml

        /// Looks up documentation by ID.
        member this.Lookup(id: Id) =
            match this.data.TryGetValue(string id) with
            | true, v -> Some v
            | _ -> None

        /// Looks up documentation for a type.
        member this.Type(tR: TypeReference) =
            this.Lookup(Id.Type tR)

        /// Looks up documentation for a method.
        member this.Method(mD: MethodReference) =
            this.Lookup(Id.Method mD)

        /// Looks up documentation for a property.
        member this.Property(pD: PropertyReference) =
            this.Lookup(Id.Property pD)

        /// Looks up documentation for a field.
        member this.Field(fD: FieldReference) =
            this.Lookup(Id.Field fD)

