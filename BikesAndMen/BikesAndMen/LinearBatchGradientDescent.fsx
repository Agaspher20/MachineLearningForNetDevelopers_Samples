#I @"..\packages\"
#r @"FSharp.Data.2.3.2\lib\net40\FSharp.Data.dll"
#load @"FSharp.Charting.0.90.14\FSharp.Charting.fsx"
#r @"MathNet.Numerics.Signed.3.13.1\lib\net40\MathNet.Numerics.dll"
#r @"MathNet.Numerics.FSharp.Signed.3.13.1\lib\net40\MathNet.Numerics.FSharp.dll"

open System.IO
open FSharp.Charting
open FSharp.Data
open MathNet
open MathNet.Numerics
open MathNet.Numerics.LinearAlgebra
open MathNet.Numerics.Providers.LinearAlgebra.Mkl
open MathNet.Numerics.LinearAlgebra.Double

System.Environment.CurrentDirectory <- Path.Combine(__SOURCE_DIRECTORY__, @"..\packages\MathNet.Numerics.MKL.Win-x86.2.1.0\build\x86")
Control.NativeProviderPath <- Path.Combine(__SOURCE_DIRECTORY__, @"..\packages\MathNet.Numerics.MKL.Win-x86.2.1.0\build\x86")
Control.UseNativeMKL()
Control.LinearAlgebraProvider <- MklLinearAlgebraProvider()

[<Literal>]
let dataPath = __SOURCE_DIRECTORY__ + @"..\..\data\day.csv"
type Data = CsvProvider<dataPath>
let dataSet = Data.Load(dataPath)
let data = dataSet.Rows

type Vec = Vector<float>
type Mat = Matrix<float>

let cost (theta:Vec) (Y:Vec) (X:Mat) =
    let ps = Y - theta * X.Transpose()
    ps * ps |> sqrt

let predict (theta:Vec) (v:Vec) = theta * v

let X = matrix [ for obs in data -> [ 1.; float obs.Instant ]]
let Y = vector [ for obs in data -> float obs.Cnt ]

let theta = vector [6000.; -4.5]
predict theta (X.Row(0))
cost theta Y X

let estimate (Y:Vec) (X:Mat) =
    (X.Transpose() * X).Inverse() * X.Transpose() * Y

let seed = 314159
let rng = System.Random(seed)

let shuffle (arr:'a []) =
    let arr = Array.copy arr
    let l = arr.Length
    for i in (l-1) .. -1 .. 1 do
        let temp = arr.[i]
        let j = rng.Next(0,i+1)
        arr.[i] <- arr.[j]
        arr.[j] <- temp
    arr

let training,validation =
    let shuffled =
        data
        |> Seq.toArray
        |> shuffle
    let size =
        0.7 * float (Array.length shuffled)
        |> int
    shuffled.[..size],
    shuffled.[size+1..]

type Obs = Data.Row
type Model = Obs -> float
type Featurizer = Obs -> float list

let predictor (f:Featurizer) (theta:Vec) =
    f >> vector >> (*) theta

let evaluate (model:Model) (data:Obs seq) =
    data
    |> Seq.averageBy (fun obs ->
        abs (model obs - float obs.Cnt))

let model (f:Featurizer) (data:Obs seq) =
    let Yt, Xt =
        data
        |> Seq.toList
        |> List.map (fun obs -> float obs.Cnt, f obs)
        |> List.unzip
    let theta = estimate (vector Yt) (matrix Xt)
    let predict = predictor f theta
    theta,predict

let featurizer0 (obs:Obs) =
    [   1.;
        float obs.Instant; ]

let (theta0,model0) = model featurizer0 training

evaluate model0 training |> printfn "Training: %.0f"
evaluate model0 validation |> printfn "Validation: %.0f" 

Chart.Combine [
    Chart.Line [ for obs in data -> float obs.Cnt ]
    Chart.Line [ for obs in data -> model0 obs ] ]


let featurizer1 (obs:Obs) =
    [   1.
        obs.Instant |> float
        obs.Atemp |> float
        obs.Hum |> float
        obs.Temp |> float
        obs.Windspeed |> float
    ] 
let (theta1,model1) = model featurizer1 training 

evaluate model1 training |> printfn "Training: %.0f"
evaluate model1 validation |> printfn "Validation: %.0f" 

Chart.Point [ for obs in data -> float obs.Cnt, model1 obs ]

Chart.Combine [
    Chart.Line [ for obs in data -> float obs.Cnt ]
    Chart.Line [ for obs in data -> model1 obs ] ]
