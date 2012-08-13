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

open System.Diagnostics
open System.IO
open System.Reflection
open Mono.Cecil
open RazorEngine
open IntelliFactory.Documentation

/// Represents parsed command-line arguments.
type Arguments =
    {
        AssemblySet : CodeModel.AssemblySet
        OutputPath : string
        RazorSkin : option<string>
    }

/// The trace source object for diagnostics.
let Trace = new TraceSource("if-doc")

/// Traces an exception.
let TraceError (code: int) (e: obj) =
    Trace.TraceEvent(TraceEventType.Error, code, e.ToString())

/// Tries to read an assembly.
let TryReadAssembly (path: string) =
    
    //bentayloruk - For some reason GlobalAssemblyResolver is "not defined" even though is in Mono.Cecil ns.
    //bentayloruk - ILSpy shows it just news a DefaultAssemblyResolver so doing this for now.
    let res = new DefaultAssemblyResolver()//let res = GlobalAssemblyResolver.Instance

    try
        Some (AssemblyDefinition.ReadAssembly path)
    with e ->
        eprintfn "Failed to read assembly: %s" path
        TraceError 0 e
        None
    |> Option.map (fun def ->
        let xml = Path.ChangeExtension(path, ".xml")
        let doc =
            if File.Exists xml then
                try
                    XmlDoc.Document.FromFile xml
                with e ->
                    eprintfn "Ignoring invalid XML: %s" xml
                    TraceError 1 e
                    XmlDoc.Document.Empty
            else
                XmlDoc.Document.Empty
        CodeParser.ParseAssembly def doc)

/// Tries to compile a Razor template.
let TryCompileRazor<'T> (name: string) (template: string) =
    try
        Razor.Compile(template, typeof<'T>, name)
        true
    with
    | :? Templating.TemplateCompilationException as e ->
        for x in e.Errors do
            TraceError 2 x
        false
    | :? Templating.TemplateParsingException as e ->
        let msg = sprintf "Line:%u Col:%u Exception:%s" e.Line e.Column (e.ToString())
        TraceError 4 msg 
        false
    | e ->
        TraceError 3 e
        false

let compile<'T> name template =
    let a = typeof<XmlDoc.Document>.Assembly
    use r = new StreamReader(a.GetManifestResourceStream template)
    if TryCompileRazor<'T> name (r.ReadToEnd()) then
        Some name
    else
        None


/// Compiles the default Razor template.
let YuiRazorSkin = compile<CodeModel.AssemblySet> "default" "Style.cshtml"

/// Prints usage information.
let Usage () =
    eprintfn "Usage: if-doc.exe [assemblies]"
    eprintfn "Arguments:"
    eprintfn "    -out path    Sets the output path."
    eprintfn "    -skin path   Sets the Razor skin."

/// Parses command-line arguments.
let Parse (args: seq<string>) =
    let rec parse res = function
        | [] ->
            res
        | "-out" :: path :: paths ->
            parse { res with OutputPath = path } paths
        | [path] as paths when res.OutputPath = null ->
            let path = Path.GetFullPath path
            let op   = Path.Combine(Path.GetDirectoryName path, "api.html")
            let res  = { res with OutputPath = op }
            parse res paths
        | "-skin" :: path :: paths ->
            if File.Exists path then
                if TryCompileRazor "user" (File.ReadAllText path) then
                    parse { res with RazorSkin = Some "user" } paths
                else
                    eprintfn "Ignoring invalid Razor template: %s" path
                    parse res paths
            else
                eprintfn "Ignoring invalid path: %s" path
                parse res paths
        | path :: paths ->
            if File.Exists path then
                match TryReadAssembly path with
                | None ->
                    parse res paths
                | Some assembly ->
                    let assemblies = assembly :: res.AssemblySet.Assemblies
                    let res = { res with AssemblySet = CodeModel.AssemblySet(assemblies) }
                    parse res paths
            else
                eprintfn "Ignoring invalid path: %s" path
                parse res paths
    let def =
        {
            OutputPath = null
            AssemblySet = CodeModel.AssemblySet([])
            RazorSkin = YuiRazorSkin
        }
    Seq.toList args
    |> parse def

/// The main program entry point.
[<EntryPoint>]
let Main args =
    
    let (++) a b = Path.Combine(a, b)

    if YuiRazorSkin.IsNone then
        eprintfn "Internal error: invalid default Razor skin."
        1
    else
        let opts = Parse args
        let skin = opts.RazorSkin.Value
        if opts.AssemblySet.Assemblies.IsEmpty then
            eprintfn "No assemblies given."
            Usage ()
            1
        else
            try
                let path = Path.GetFullPath opts.OutputPath
                let dir = Path.GetDirectoryName path
                if not (Directory.Exists dir) then
                    Directory.CreateDirectory dir
                    |> ignore
                let res = Razor.Run(skin, opts.AssemblySet)
                File.WriteAllText(path, res)

                0
            with e ->
                eprintfn "Internal error."
                1
