#I @"..\packages\"
#r @"FSharp.Charting.0.90.14\lib\net40\FSharp.Charting.dll"
#load @"FSharp.Charting.0.90.14\FSharp.Charting.fsx" 
#load "KMeans.fs"

open System
open System.IO
open FSharp.Charting
open KMeans

let generalPath = __SOURCE_DIRECTORY__ + @"..\..\data\";
let file = "userprofiles-toptags.txt"
let headers,observations = 
    let raw =
        generalPath + file
        |> File.ReadAllLines 
    // first row is headers, first col is user ID
    let headers = (raw.[0].Split ',').[1..] 
    let observations =
        raw.[1..]
        |> Array.map (fun line -> (line.Split ',').[1..])
        |> Array.map (Array.map float)
    headers,observations
let labels = ChartTypes.LabelStyle(Interval=0.25) 

type Observation = float []

let features = headers.Length

let distance (obs1:Observation) (obs2:Observation) =
    (obs1, obs2)
    ||> Seq.map2 (fun u1 u2 -> pown (u1 - u2) 2)
    |> Seq.sum 

let centroidOf (cluster:Observation seq) =
    Array.init features (fun f ->
        cluster
        |> Seq.averageBy (fun user -> user.[f]))

let observations1 =
    observations
    |> Array.map (Array.map float)
    |> Array.filter (fun x -> Array.sum x > 0.)
let (clusters1, classifier1) =
    let clustering = clusterize distance centroidOf
    let k = 5
    clustering observations1 k

clusters1 |> Seq.iter (fun (id,profile) ->
    printfn "CLUSTER %i" id
    profile
    |> Array.iteri (fun i value -> printfn "%16s %.1f" headers.[i] value))

Chart.Combine [
    for (id,profile) in clusters1 ->
        profile
        |> Seq.mapi (fun i value -> headers.[i], value)
        |> Chart.Bar
    ]
|> fun chart -> chart.WithXAxis(LabelStyle=labels)

observations1
|> Seq.countBy (fun obs -> classifier1 obs)
|> Seq.iter (fun (clusterID, count) ->
    printfn "Cluster %i: %i elements" clusterID count)

let rowNormalizer (obs:Observation) : Observation =
    let max = obs |> Seq.max
    obs |> Array.map (fun tagUse -> tagUse / max) 
let observations2 =
    observations
        |> Array.filter (fun x -> Array.sum x > 0.)
        |> Array.map (Array.map float)
        |> Array.map rowNormalizer 

let (clusters2, classifier2) =
    let clustering = clusterize distance centroidOf
    let k = 5
    clustering observations2 k

observations2
|> Seq.countBy (fun obs -> classifier2 obs) 
|> Seq.iter (fun (clusterID, count) ->
    printfn "Cluster %i: %i elements" clusterID count)

Chart.Combine [
    for (id,profile) in clusters2 ->
        profile
        |> Seq.mapi (fun i value -> headers.[i], value)
        |> Chart.Bar
    ]
|> fun chart -> chart.WithXAxis(LabelStyle=labels)

let ruleOfThumb (n:int) = sqrt (float n / 2.)
let k_ruleOfThumb = ruleOfThumb (observations2.Length)

let squareError (obs1:Observation) (obs2:Observation) =
    (obs1,obs2)
    ||> Seq.zip
    |> Seq.sumBy (fun (x1,x2) -> pown (x1-x2) 2)

let RSS (dataset:Observation[]) centroids =
    dataset
    |> Seq.sumBy (fun obs ->
        centroids
        |> Seq.map (squareError obs)
        |> Seq.min)

let AIC (dataset:Observation[]) centroids =
    let k = centroids |> Seq.length
    let m = dataset.[0] |> Seq.length
    RSS dataset centroids + float (2 * m * k)

let assessments =
    [1..25]
    |> Seq.map (fun k ->
        let value =
            [ for _ in 1 .. 10 ->
                let (clusters, classifier) =
                    let clustering = clusterize distance centroidOf
                    clustering observations2 k
                AIC observations2 (clusters |> Seq.map snd) ]
            |> List.average
        k, value)
    |> Seq.toArray

assessments |> Chart.Line

let (bestClusters, bestClassifier) =
    let clustering = clusterize distance centroidOf
    let k = 11
    seq {
        for _ in 1 .. 20 ->
            clustering observations2 k
    }
    |> Seq.minBy (fun (cs,f) ->
        RSS observations2 (cs |> Seq.map snd))

bestClusters
|> Seq.iter (fun (id,profile) ->
    printfn "CLUSTER %i" id
    profile
    |> Array.iteri (fun i value ->
        if value > 0.2 then printfn "%16s %.1f" headers.[i] value))
