#I @"..\packages\"
#r @"FSharp.Data.2.3.2\lib\net40\FSharp.Data.dll"
#r @"Deedle.1.2.5\lib\net40\Deedle.dll"
#r @"R.NET.Community.1.6.5\lib\net40\RDotNet.dll"
#r @"RProvider.1.1.20\lib\net40\RProvider.Runtime.dll"
#r @"RProvider.1.1.20\lib\net40\RProvider.dll"
#r @"Deedle.RPlugin.1.2.5\lib\net40\Deedle.RProvider.Plugin.dll"

open FSharp.Data
open Deedle
open RProvider
open RProvider.``base``
open Deedle.RPlugin
open RProvider.rworldmap

let wb = WorldBankData.GetDataContext ()
let countries = wb.Countries

let population2000 =
    series [ for c in countries -> c.Code, c.Indicators.``Population, total``.[2000]]
let population2010 =
    series [ for c in countries -> c.Code, c.Indicators.``Population, total``.[2010]]
let surface =
    series [ for c in countries -> c.Code, c.Indicators.``Surface area (sq. km)``.[2010]]

let dataframe =
    frame [
        "Pop2000", population2000
        "Pop2010", population2010
        "Surface", surface ]
dataframe?Code <- dataframe.RowKeys
dataframe?Density <- dataframe?Pop2010 / dataframe?Surface 

let map = R.joinCountryData2Map(dataframe,"ISO3","Code")
R.mapCountryData(map,"Pop2000")


map = R.joinCountryData2Map(dataframe,"ISO3","Code")
R.mapCountryData(map,"Density")