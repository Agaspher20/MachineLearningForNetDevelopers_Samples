#I @"..\packages\"
#r @"MathNet.Numerics.3.13.1\lib\net40\MathNet.Numerics.dll"
#r @"MathNet.Numerics.FSharp.3.13.1\lib\net40\MathNet.Numerics.FSharp.dll" 
#r @"FSharp.Charting.0.90.14\lib\net40\FSharp.Charting.dll"

#load @"FSharp.Charting.0.90.14\FSharp.Charting.fsx" 
#load "KMeans.fs"
#load "PrincipalComponentAnalysis.fs"

open System
open System.IO

open MathNet
open MathNet.Numerics.LinearAlgebra
open MathNet.Numerics.Statistics 

open FSharp.Charting

open KMeans
open PrincipalComponentAnalysis

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

let correlations =
    observations
    |> Matrix.Build.DenseOfColumnArrays
    |> Matrix.toRowArrays
    |> Correlation.PearsonMatrix 

let features = headers.Length
let correlated =
    [
        for col in 0 .. (features - 1) do
            for row in (col + 1) .. (features - 1) ->
                headers.[col], headers.[row], correlations.[col,row]
    ]
    |> Seq.sortByDescending (fun (f1, f2, corr) -> abs corr)
    |> Seq.take 20
    |> Seq.iter (fun (f1, f2, corr) -> printfn "%s %s : %.2f" f1 f2 corr) 

let normalized = PrincipalComponentAnalysis.normalize (headers.Length) observations 
let (eValues,eVectors), projector = principalComponentAnalysis normalized

let total = eValues |> Seq.sumBy (fun x -> x.Magnitude)

eValues
|> Vector.toList
|> List.rev
|> List.scan (fun (percent,cumul) value ->
    let percent = 100. * value.Magnitude / total
    let cumul = cumul + percent
    (percent,cumul)) (0.,0.)
|> List.tail
|> List.iteri (fun i (p,c) -> printfn "Feature %2i: %.2f%% (%.2f%%)" i p c)

let principalComponent comp1 comp2 =
    let title = sprintf "Component %i vs %i" comp1 comp2
    let features = headers.Length
    let coords = Seq.zip (eVectors.Column(features-comp1)) (eVectors.Column(features-comp2))
    Chart.Point (coords, Title = title, Labels = headers, MarkerSize = 7)
    |> Chart.WithXAxis(Min = -1.0, Max = 1.0,
        MajorGrid = ChartTypes.Grid(Interval = 0.25),
        LabelStyle = ChartTypes.LabelStyle(Interval = 0.25),
        MajorTickMark = ChartTypes.TickMark(Enabled = false))
    |> Chart.WithYAxis(Min = -1.0, Max = 1.0,
        MajorGrid = ChartTypes.Grid(Interval = 0.25),
        LabelStyle = ChartTypes.LabelStyle(Interval = 0.25),
        MajorTickMark = ChartTypes.TickMark(Enabled = false))

principalComponent 3 4

let projections comp1 comp2 =
    let title = sprintf "Component %i vs %i" comp1 comp2
    let features = headers.Length
    let coords =
        normalized
        |> Seq.map projector
        |> Seq.map (fun obs -> obs.[features-comp1], obs.[features-comp2])
    Chart.Point (coords, Title = title)
    |> Chart.WithXAxis(Min = -200.0, Max = 500.0,
        MajorGrid = ChartTypes.Grid(Interval = 100.),
        LabelStyle = ChartTypes.LabelStyle(Interval = 100.),
        MajorTickMark = ChartTypes.TickMark(Enabled = false))
    |> Chart.WithYAxis(Min = -200.0, Max = 500.0,
        MajorGrid = ChartTypes.Grid(Interval = 100.),
        LabelStyle = ChartTypes.LabelStyle(Interval = 100.),
        MajorTickMark = ChartTypes.TickMark(Enabled = false))

projections 3 4
