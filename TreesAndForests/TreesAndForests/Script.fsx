#I @"..\packages\"
#r @"FSharp.Data.2.3.2\lib\net40\FSharp.Data.dll"

open FSharp.Data

[<Literal>]
let dataPath = __SOURCE_DIRECTORY__ + @"..\..\data\titanic.csv"

type Titanic = CsvProvider<dataPath>
type Passenger = Titanic.Row

let dataset = Titanic.GetSample ()

dataset.Rows
|> Seq.countBy (fun passenger -> passenger.Survived)
|> Seq.iter (printfn "%A")

dataset.Rows
|> Seq.averageBy (fun passenger ->
    if passenger.Survived then 1.0 else 0.0)
|> printfn "Chances of survival: %.3f"

let survivalRate (passengers:Passenger seq) =
    let total = passengers |> Seq.length
    let survivors =
        passengers
        |> Seq.filter (fun p -> p.Survived)
        |> Seq.length
    100.0 * (float survivors / float total)

let bySex =
    dataset.Rows
    |> Seq.groupBy(fun p -> p.Sex)

bySex |> Seq.iter (fun (s,g) ->
    printfn "Sex %A: %f" s (survivalRate g))

let byClass =
    dataset.Rows
    |> Seq.groupBy (fun p -> p.Pclass)

byClass |> Seq.iter (fun (s,g) ->
    printfn "Class %A: %f" s (survivalRate g))

#load "DecisionStump.fs"
open DecisionStump

let survived (p:Passenger) = p.Survived
let validate (featureName:string) classifier (rows:seq<Passenger>) =
    printfn "Stump: classify based on %s." featureName
    rows
    |> Seq.averageBy (fun p ->
        if p.Survived = classifier p then 1.0 else 0.0)


let sex (p:Passenger) = p.Sex
let sexClassifier = survived |> learn (dataset.Rows) sex

dataset.Rows |> validate "passenger sex" sexClassifier

let classFeature (p:Passenger) = p.Pclass
let classClassifier = learn (dataset.Rows) classFeature survived

dataset.Rows |> validate "passenger class" classClassifier

dataset.Rows
|> Seq.groupBy (fun p -> p.Fare)
|> Seq.iter(fun (price, passengers) ->
    printfn "%6.2F: %6.2f" price (survivalRate passengers))

let averageFare =
    dataset.Rows
    |> Seq.averageBy (fun p -> p.Fare)
let fareFeature (p:Passenger) =
    if p.Fare < averageFare
    then "Cheap"
    else "Expensive"
let fareClassifier = learn (dataset.Rows) fareFeature survived

dataset.Rows |> validate "fare level" fareClassifier

dataset.Rows
|> Seq.groupBy (fun p -> p.Embarked)
|> Seq.iter (fun (port, passengers) ->
    printfn "%s: %f" port (survivalRate passengers))

let port (p:Passenger) =
    if p.Embarked = "" then None
    else Some(p.Embarked) 
let portClassifier = survived |> betterLearn (dataset.Rows) port

dataset.Rows |> validate "embarked port" portClassifier
