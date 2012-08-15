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
open IntelliFactory.Documentation
open Nustache.Core
open FakeModels

/// Represents parsed command-line arguments.
type Arguments =
    {
        AssemblySet : CodeModel.AssemblySet
        OutputPath : string
        SkinPaths : string list 
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

/// Prints usage information.
let Usage () =
    eprintfn "Usage: if-doc.exe [assemblies]"
    eprintfn "Arguments:"
    eprintfn "    -out path    Sets the output path."
    eprintfn "    -skin path   Sets a skin (supports multiple)."

/// Parses command-line arguments.
let Parse (args: seq<string>) =
    let rec parse res = function
        | [] ->
            res
        | "-out" :: path :: paths ->
            parse { res with OutputPath = path } paths
        | "-skin" :: path :: paths ->
            if File.Exists path then
                parse { res with SkinPaths = path :: res.SkinPaths } paths
            else
                eprintfn "Skin file does not exist at path: %s" path
                failwith "Bad skin path"
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
                eprintfn "Assembly path does not exist: %s" path
                failwith "Bad skin path"
    let def =
        {
            OutputPath = null
            AssemblySet = CodeModel.AssemblySet([])
            SkinPaths = []
        }
    let opts = Seq.toList args |> parse def
    if opts.OutputPath = null then failwith "Out path not specified." else opts

/// The main program entry point.
[<EntryPoint>]
let Main args =
    
    let (++) a b = Path.Combine(a, b)

    let opts = Parse args
    if opts.AssemblySet.Assemblies.IsEmpty then
        eprintfn "No assemblies given."
        Usage ()
        1
    else
        try
            let dir = opts.OutputPath
            if not (Directory.Exists dir) then
                Directory.CreateDirectory dir
                |> ignore

            for skinPath in opts.SkinPaths do
                let model = fakeViews (Path.GetFileNameWithoutExtension(skinPath)) opts.AssemblySet
                let newFileName = (Path.GetFileNameWithoutExtension(skinPath) + ".html").ToLower()
                let renderPath = Path.Combine(dir, newFileName).ToLower()
                let render = Render.FileToString(skinPath, model.Add("filename", newFileName))
                File.WriteAllText(renderPath, render)
            0
        with e ->
            eprintfn "Internal error %s." e.Message
            let _ = System.Console.ReadLine()
            1
