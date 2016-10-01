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

Chart.Line [ for obs in data -> obs.Cnt ]

let ma n (series:float seq) =
    series
    |> Seq.windowed n
    |> Seq.map (fun xs -> xs |> Seq.average)
    |> Seq.toList

let count = seq { for obs in data -> float(obs.Cnt) }
Chart.Combine [
    Chart.Line count
    Chart.Line (ma 7 count)
    Chart.Line (ma 30 count) ]