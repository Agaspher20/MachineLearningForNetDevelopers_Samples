﻿#I @"..\packages\"
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

let model (theta0, theta1) (obs:Observation) = 
    theta0 + theta1 * (float obs.Instant)

type Model = Observation -> float

let count = seq { for obs in data -> float(obs.Cnt) }
let cost (observations:Observation seq) (model:Model) = 
    observations
    |> Seq.sumBy (fun x -> pown (float x.Cnt - model x) 2)
    |> sqrt

let overallCost = cost data

let update alpha (theta0, theta1) (obs:Observation) =
    let count = float obs.Cnt
    let instant = float obs.Instant
    let theta0' = theta0 - 2. * alpha * 1. * (theta0 + theta1 * instant - count)
    let theta1' = theta1 - 2. * alpha * instant *  (theta0 + theta1 * instant - count)
    theta0', theta1'

let obs100 = data |> Seq.item 100
let testUpdate = update 0.00001 (0.,0.) obs100
cost [obs100] (model (0.,0.))
cost [obs100] (model testUpdate)

let stochastic rate (theta0,theta1) inputData =
    inputData
    |> Seq.fold (fun (t0,t1) obs ->
        printfn "%.4f,%.4f" t0 t1
        update rate (t0,t1) obs) (theta0,theta1)

let tune_rate =
    [ for r in 1 .. 20 ->
        (pown 0.1 r), data |> stochastic (pown 0.1 r) (0.,0.) |> model |> overallCost ]

let tunedRate = pown 0.1 8
let tunedModel = model (stochastic tunedRate (0.,0.) data)
Chart.Combine [
    Chart.Line count
    Chart.Line [ for obs in data -> tunedModel obs ] ]

let hiRate = 10.0 * tunedRate
let error_eval =
    data
    |> Seq.scan (fun (t0,t1) obs -> update hiRate (t0,t1) obs) (0.0,0.0)
    |> Seq.map (model >> overallCost)
    |> Chart.Line
