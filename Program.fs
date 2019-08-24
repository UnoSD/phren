open System
open System.Collections.Generic
open System.Globalization
open MetadataExtractor
open MetadataExtractor.Formats.Exif
open System.IO

[<EntryPoint>]
let main argv =
    
    let (|File|Folder|) path =
        match path with
        | x when File.Exists(x) -> File x
        | x when Directory.Exists(x) -> Folder x
        | x -> failwithf "Invalid file or direcyory: %s" x 
    
    let getFilesFrom folder =
        Directory.EnumerateFiles(folder) |> Seq.toList
    
    let getFiles path =
        match path with
        | File file -> [ file ]
        | Folder folder -> getFilesFrom folder
    
    let getTimestamp (data : IReadOnlyList<MetadataExtractor.Directory>) =
        data |>
        Seq.choose (fun d -> match d with
                             | :? ExifSubIfdDirectory as x ->
                                 x.GetDescription(ExifDirectoryBase.TagDateTimeOriginal) |>
                                 Some
                             | _ -> None) |>
        Seq.map (fun d -> DateTime.ParseExact(d, "yyyy:MM:dd HH:mm:ss", null)) |>
        Seq.exactlyOne
    
    let formatDate (dateTime : DateTime) =
        dateTime.ToString("dd-MMM-yyyy", new CultureInfo("it-IT"))
    
    let getNewFileName fileName (extension : string) formattedDate =
        sprintf "%s.%s%s" formattedDate fileName (extension.ToLower()) //fileName 
    
    let files =
        argv |>
        Array.fold (fun x y -> getFiles y @ x) [] |>
        List.map (fun fileName -> (new FileInfo(fileName), ImageMetadataReader.ReadMetadata(fileName))) |>
        List.map (fun (file, data) -> (file, getTimestamp data)) |>
        List.map (fun (file, dateTime) -> (file,
                                           dateTime,
                                           getNewFileName (Path.GetFileNameWithoutExtension(file.Name))
                                                          file.Extension
                                                          (dateTime |> formatDate))) |>
        List.sortBy (fun (_, dateTime, _) -> dateTime)
    
    let updateKvp (map : Map<DateTime, ('a * DateTime * 'c) list>) fileInfo dateTime newName =
        // More C#-ish, but cleaner
        
        // maxBy instead of sort and then pick...
        
        let (referenceDateTime, list) = (map |> Seq.sortByDescending (fun kvp -> kvp.Key) |> Seq.head).Deconstruct()
        // More idiomatic but more boilerplate
        //let (y, z) = match (map |> Seq.sortByDescending (fun kvp -> kvp.Key) |> Seq.head) with | KeyValue (a, b) -> (a, b) 
        // Idiomatic and less boilerplate
        let (KeyValue(referenceDateTime, list)) = map |> Seq.sortByDescending (fun kvp -> kvp.Key) |> Seq.head
        
        map |>
        Map.remove referenceDateTime |>
        Map.add dateTime ((fileInfo, dateTime, newName) :: list)
    
    let (|Empty|NonEmpty|) map =
        match Map.count map with
        | 0 -> Empty
        | _ -> NonEmpty map
    
    let lastDateAddedIsAwayFrom dateTime (map : Map<DateTime, 'b>) =
        map |>
        Seq.sortByDescending (fun kvp -> kvp.Key) |>
        Seq.head |>
        (function KeyValue (key, _) -> dateTime - key > TimeSpan.FromHours(1.))
    
    let updateMap map fileInfo dateTime newName =
        match map with
        | Empty                                              -> Map [(dateTime, [(fileInfo, dateTime, newName)])]
        | NonEmpty map when map |>
                            lastDateAddedIsAwayFrom dateTime -> map.Add (dateTime, [(fileInfo, dateTime, newName)])
        | NonEmpty map                                       -> updateKvp map fileInfo dateTime newName
    
    let processImageGroup (list : (FileInfo * DateTime * string) list) =
        let maxDate = list |> List.maxBy (fun (_, dateTime, _) -> dateTime) |> (fun (_, x, _) -> x)
        let minDate = list |> List.minBy (fun (_, dateTime, _) -> dateTime) |> (fun (_, x, _) -> x)
        
        let daysString =
            match minDate.Day = maxDate.Day with
            | true -> minDate.ToString("dd-MMM-yyyy", new CultureInfo("it-IT")) |> string
            | false -> maxDate.ToString("dd-MMM-yyyy", new CultureInfo("it-IT")) |> sprintf "%i-%s" minDate.Day 
            
        let (fileInfo, _, _) = list.Head
        
        let getNewPath newName =
            let basePath =
                match list with
                //| [] | [_] -> 
                | _ :: _ :: _ -> let folderPath = Path.Combine(fileInfo.Directory.FullName, daysString)
                                 if Directory.Exists(folderPath) then
                                    ()
                                 else
                                    Directory.CreateDirectory(folderPath) |> ignore
                                 folderPath
                | _ -> fileInfo.Directory.FullName 
            
            Path.Combine(basePath, newName)
        
        list |>
        List.map (fun (fileInfo, _, newName) -> (fileInfo, getNewPath newName)) |>
        //List.iter (fun (fileInfo, newPath) -> printfn "%A -> %A" fileInfo.Name newPath)
        List.iter (fun (fileInfo, newPath) -> fileInfo.MoveTo(newPath))
    
    files |>
    List.fold (fun map (fileInfo, dateTime, newName) -> updateMap map fileInfo dateTime newName) Map.empty |>
    Seq.map (function KeyValue (_, v) -> v) |>
    Seq.iter processImageGroup
    
    
    //file.MoveTo(newFilename)
    //File.Move(file.FullName, Path.Combine(file.DirectoryName, newFilename))
    
    0
