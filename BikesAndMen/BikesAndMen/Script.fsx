#I @"..\packages\"
#r @"FSharp.Data.2.3.2\lib\net40\FSharp.Data.dll" 

open FSharp.Data

[<Literal>]
let dataPath = __SOURCE_DIRECTORY__ + @"..\..\data\day.csv"
type Data = CsvProvider<dataPath>
let dataSet = Data.Load(dataPath)
let data = dataSet.Rows