open System
open System.Collections.Generic
open System.Globalization
open MetadataExtractor
open MetadataExtractor.Formats.Exif
open System.IO

type Config =
    {
        culture : string
        format : string
        threshold : TimeSpan
        dryRun : bool
    }

let config =
    {//{|
        culture = "it-IT"
        format = "dd-MMM-yyyy"
        threshold = TimeSpan.FromHours(1.)
        dryRun = false
    }//|}

let multipleDaysFormat (date : DateTime) format = sprintf "%i-%s" date.Day format

type Metadata = MetadataExtractor.Directory

[<EntryPoint>]
let main argv =
    
    let (|File|Folder|) path =
        match path with
        | x when File.Exists(x)      -> File x
        | x when Directory.Exists(x) -> Folder x
        | x                          -> failwithf "Invalid file or direcyory: %s" x 
    
    let getFiles path =
        match path with
        | File file     -> [ file ]
        | Folder folder -> Directory.EnumerateFiles(folder) |> Seq.toList
    
    let getTimestamp (data : IReadOnlyList<Metadata>) =
        data |>
        Seq.choose (fun d -> match d with
                             | :? ExifSubIfdDirectory as x ->
                                 x.GetDescription(ExifDirectoryBase.TagDateTimeOriginal) |>
                                 Some
                             | _ -> None) |>
        Seq.map (fun d -> DateTime.ParseExact(d, "yyyy:MM:dd HH:mm:ss", null)) |>
        Seq.exactlyOne
    
    let formatDate (dateTime : DateTime) =
        dateTime.ToString(config.format, new CultureInfo(config.culture))
    
    let getNewFileName (fileInfo : FileInfo) formattedDate =
        fileInfo.Extension.ToLower() |>
        sprintf "%s.%s%s" formattedDate (fileInfo.Name |> Path.GetFileNameWithoutExtension)

    let updateFolderGroup (map : Map<DateTime, ('a * DateTime * 'b) list>) fileInfo dateTime newName =
        let (KeyValue(referenceDateTime, list)) = map |> Seq.maxBy (fun kvp -> kvp.Key)
        
        map |>
        Map.remove referenceDateTime |>
        Map.add dateTime ((fileInfo, dateTime, newName) :: list)
    
    let (|Empty|NonEmpty|) map =
        match Map.count map with
        | 0 -> Empty
        | _ -> NonEmpty map
    
    let lastDateAddedIsAwayFrom dateTime (map : Map<DateTime, 'b>) =
        map |>
        Seq.maxBy (fun kvp -> kvp.Key) |>
        (function KeyValue (key, _) -> dateTime - key > config.threshold)
    
    let addToGroups map fileInfo dateTime newName =
        match map with
        | Empty                                              -> Map [(dateTime, [(fileInfo, dateTime, newName)])]
        | NonEmpty map when map |>
                            lastDateAddedIsAwayFrom dateTime -> map.Add (dateTime, [(fileInfo, dateTime, newName)])
        | NonEmpty map                                       -> updateFolderGroup map fileInfo dateTime newName
    
    let createDirectory =
        match config.dryRun with
        | true  -> printfn "Creating directory %s"
        | false -> Directory.CreateDirectory >> ignore
        
    let moveTo path (fileInfo : FileInfo) =
        match config.dryRun with
        | true  -> printfn "%s -> %s" fileInfo.Name path
        | false -> path |> fileInfo.MoveTo
    
    let processImageGroup (list : (FileInfo * DateTime * string) list) =
        let getDateBy func =
            list |> func (fun (_, dateTime, _) -> dateTime) |> (fun (_, x, _) -> x)
        
        let maxDate = getDateBy List.maxBy
        let minDate = getDateBy List.minBy
        
        let daysString =
            match minDate.Day = maxDate.Day with
            | true  -> minDate |> formatDate
            | false -> maxDate |> formatDate |> multipleDaysFormat minDate
            
        let (fileInfo, _, _) = list.Head
        
        let getNewPath newName =
            let basePath =
                match list.Length with
                | 0 | 1 -> fileInfo.Directory.FullName
                | _ -> let folderPath = Path.Combine(fileInfo.Directory.FullName, daysString)
                       if Directory.Exists(folderPath) then
                          ()
                       else
                          createDirectory folderPath
                       folderPath
            
            Path.Combine(basePath, newName)
        
        list |>
        List.map  (fun (fileInfo, _, newName) -> (fileInfo, getNewPath newName)) |>
        List.iter (fun (fileInfo, newPath) -> fileInfo |> moveTo newPath)
    
    argv |>
    Array.fold  (fun fileList path -> getFiles path @ fileList) [] |>
    List.map    (fun fileName -> (FileInfo(fileName), ImageMetadataReader.ReadMetadata(fileName) |> getTimestamp)) |>
    List.map    (fun (file, dateTime) -> (file, dateTime, dateTime |> formatDate |> getNewFileName file)) |>
    List.sortBy (fun (_, dateTime, _) -> dateTime) |>
    List.fold   (fun map (fileInfo, dateTime, newName) -> addToGroups map fileInfo dateTime newName) Map.empty |>
    Seq.map     (function KeyValue (_, v) -> v) |>
    Seq.iter    processImageGroup
        
    0
