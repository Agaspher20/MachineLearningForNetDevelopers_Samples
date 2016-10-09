#I @"..\packages\"
#r @"FSharp.Data.2.3.2\lib\net40\FSharp.Data.dll" 

open FSharp.Data

[<Literal>]
let dataPath = __SOURCE_DIRECTORY__ + @"..\..\data\day.csv"
type Data = CsvProvider<dataPath>
let dataSet = Data.Load(dataPath)
let data = dataSet.Rows

#load @"FSharp.Charting.0.90.14\FSharp.Charting.fsx"
open FSharp.Charting

type Observation = Data.Row

let update alpha (theta0, theta1) (obs:Observation) =
    let count = float obs.Cnt
    let instant = float obs.Instant
    let theta0' = theta0 - 2. * alpha * 1. * (theta0 + theta1 * instant - count)
    let theta1' = theta1 - 2. * alpha * instant *  (theta0 + theta1 * instant - count)
    theta0', theta1'

let batchUpdate rate (theta0, theta1) (observations:Observation seq) =
    let updates =
        observations
        |> Seq.map (update rate (theta0, theta1))
    let theta0' = updates |> Seq.averageBy fst
    let theta1' = updates |> Seq.averageBy snd
    theta0', theta1'

let batch rate iters (observations:Observation seq) =
    let rec search (t0,t1) i =
        if i = 0 then (t0,t1)
        else
            search (batchUpdate rate (t0,t1) observations) (i-1)
    search (0.0,0.0) iters

let model (theta0, theta1) (obs:Observation) = 
    theta0 + theta1 * (float obs.Instant)

type Model = Observation -> float

let cost (observations:Observation seq) (model:Model) = 
    observations
    |> Seq.sumBy (fun x -> pown (float x.Cnt - model x) 2)
    |> sqrt

let overallCost = cost data

let batched_error rate =
    Seq.unfold (fun (t0,t1) ->
        let (t0',t1') = batchUpdate rate (t0,t1) data
        let err = model (t0,t1) |> overallCost
        Some(err, (t0',t1'))) (0.0,0.0)
    |> Seq.take 100
    |> Seq.toList
    |> Chart.Line

batched_error 0.000001
