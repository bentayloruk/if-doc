module FakeModels
open IntelliFactory.Documentation
open System.Collections.Generic
open System

let isDocableMember (m:CodeModel.IMember) = 
    m.Documentation.IsSome && m.Documentation.Value.IsUserDoc

let mapMembers (members:CodeModel.IMember seq) =
    members 
    |> Seq.filter isDocableMember 
    |> Seq.map (fun x -> 
        let join separator (args:_ seq) = String.Join(separator, args)

        let summary = x.Documentation.Value.Summary.Replace("div", "span") 

        let typesum = 
            //TODO use the tokens pretty print instead of this hack.
            match x with 
            | :? CodeModel.ModuleMethod as mm -> 
                mm.Parameters 
                |> Seq.fold (fun acc param ->
                    let rec typeDisplay = function
                        | CodeModel.Type.ConcreteType(tr) -> tr.DisplayName
                        | CodeModel.Type.FunctionType(t1, t2) -> 
                            //if x.Documentation.Value.Defs.ContainsKey(param.Name) then
                            //   "Yo" |> ignore
                            "(" + typeDisplay t1 + " -> " + typeDisplay t2 + ")"//not convinced about my bracketing here.
                        | _ -> "TBD"
                    let text = typeDisplay param.Type
                    (*
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
                    *)
                    let paramComments = x.Documentation.Value.Parameters.[param.Name]
                    let xxx = sprintf "<span rel='popover' data-content='%s' data-original-title='%s'>%s</span>" paramComments text param.Name
                    acc + xxx + ":<span style='color:gray'>" + text + "</span> "
                ) ""
            | _ -> ""

        let ps = 
            x.Documentation.Value.Parameters 
            |> Seq.map (fun p -> p.Key + " " + p.Value) 
            |> (join "<br/>")

        [
        "name", x.Name :> obj;
        "summary", summary :> obj
        "params", ps :> obj
        "typesum", typesum :> obj
        ] |> Map.ofList
        )

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
    
