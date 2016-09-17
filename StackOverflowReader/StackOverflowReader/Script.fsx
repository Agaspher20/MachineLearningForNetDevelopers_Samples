﻿#I @"..\packages\"

#r @"FSharp.Data.2.3.2\lib\net40\FSharp.Data.dll"

#r @"R.NET.Community.1.6.5\lib\net40\RDotNet.dll"
#r @"RProvider.1.1.20\lib\net40\RProvider.Runtime.dll"
#r @"RProvider.1.1.20\lib\net40\RProvider.dll" 

open FSharp.Data
open RProvider
open RProvider.``base``
open RProvider.graphics

let wb = WorldBankData.GetDataContext()
let countries = wb.Countries
let pop2000 = [for c in countries -> c.Indicators.``Population, total``.[2000]]
let pop2010 = [for c in countries -> c.Indicators.``Population, total``.[2010]]
let surface = [for c in countries -> c.Indicators.``Surface area (sq. km)``.[2010]]
R.summary(surface) |> R.print
R.hist(surface |> Seq.map log)
R.hist(surface |> R.log)
R.plot(surface, pop2010)

let pollution =
    [for c in countries -> c.Indicators.``CO2 emissions (kt)``.[2000]]
let education =
    [for c in countries ->
        c.Indicators.``School enrollment, secondary (gross), gender parity index (GPI)``.[2000]] 
let rdf =
    [ "Pop2000", box pop2000
      "Pop2010", box pop2010
      "Surface", box surface 
      "Pollution", box pollution
      "Education", box education ]
    |> namedParams
    |> R.data_frame 
