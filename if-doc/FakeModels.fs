module FakeModels
open IntelliFactory.Documentation
open System.Collections.Generic
open System

let (memberFilters:(CodeModel.IMember->bool) list) = [
    (fun m -> m.Documentation.IsNone);
    (fun m -> String.IsNullOrEmpty(m.Documentation.Value.Summary));//hack as option can be null?
    (fun m -> Char.IsLower(m.Name.[0]));
]

let isDocableMember (m:CodeModel.IMember) = 
    not (memberFilters |> Seq.exists (fun mf -> mf m))

let mapMembers (members:CodeModel.IMember seq) =
    members 
    |> Seq.filter isDocableMember 
    |> Seq.map (fun x -> 
        //TODO stop if-doc from html-izing in the first place
        let summary = x.Documentation.Value.Summary.Replace("div", "span") 
        let argJoiner (args:_ seq) = String.Join("<br/>", args)
        let ps = x.Documentation.Value.Parameters |> Seq.map (fun p -> p.Key + " " + p.Value) |> argJoiner
        [
        "name", x.Name :> obj;
        "summary", summary :> obj
        "params", ps :> obj
        ] |> Map.ofList)

let mapTypes (types:CodeModel.IType seq) =
    types 
    |> Seq.filter (fun t -> t.Methods |> Seq.filter isDocableMember |> Seq.length > 0)
    |> Seq.map (fun x -> 
        let name = x.Name.Replace("Helper", "")
        ["name", name :> obj; "functions", (mapMembers x.Methods) :> obj] |> Map.ofList)
    
let mapNs (namespaces:CodeModel.Namespace list) =
    namespaces 
    |> Seq.map (fun x -> 
        let docTypes = (mapTypes x.Types)
        //let nonDocTypes = x.Types |> Seq.filter (fun t -> docTypes |> Seq.exists (fun dt -> dt.Id = t.Id) )
        ["name", x.Name :> obj; "types", docTypes :> obj;] |> Map.ofList)

let indexView (assSet:CodeModel.AssemblySet) = 
    let asses = 
        assSet.Assemblies 
        |> List.rev //as the if-doc args parse reverses the command line order
        |> Seq.map (fun ass -> 
            [
            "name", ass.Name :> obj; 
            "namespaces", (mapNs ass.Namespaces) :> obj;
            ] 
            |> Map.ofList
        )
    [
    "assemblies", asses :> obj;
    "title", "FAKE - Build Automation for .NET" :> obj; 
    ] 
    |> Map.ofList

let fakeViews (name:string) (assSet:CodeModel.AssemblySet) = 
    let viewMap = ["index", indexView] |> Map.ofList
    (viewMap.Item (name.ToLower())) assSet
    
