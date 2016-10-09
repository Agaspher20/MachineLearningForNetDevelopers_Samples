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

