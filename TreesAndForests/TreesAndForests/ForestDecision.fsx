#I @"..\packages\"
#r @"FSharp.Data.2.3.2\lib\net40\FSharp.Data.dll"
#load "DecisionStump.fs"
#load "Tree.fs"
#load "Forest.fs"
#load "KFold.fs"

open DecisionStump
open Tree
open Forest
open KFold
open FSharp.Data
open System

[<Literal>]
let dataPath = __SOURCE_DIRECTORY__ + @"..\..\data\titanic.csv"

type Titanic = CsvProvider<dataPath>
type Passenger = Titanic.Row

let dataset = Titanic.GetSample ()

let label (p:Passenger) = p.Survived
let forestFeatures = [
    "Sex", fun (p:Passenger) -> p.Sex |> Some
    "Class", fun p -> p.Pclass |> string |> Some
    "Age", fun p -> if p.Age < 7.0 then Some("Younger") else Some("Older")
    "Port", fun p -> if p.Embarked = "" then None else Some(p.Embarked) ]

let forestResults () =
    let folds =
        dataset.Rows
        |> Seq.toArray
        |> kfold 10 
    let accuracy predictor (sample:Passenger seq) =
        sample
        |> Seq.averageBy (fun p ->
            if p.Survived = predictor p then 1.0 else 0.0) 
    [for (training,validation) in folds ->
        let forest = growForest 1000 training label forestFeatures
        let accuracyTraining = accuracy forest training
        let accuracyValidation = accuracy forest validation 
        printfn "Training: %.3f, Validation: %.3f" accuracyTraining accuracyValidation
        accuracyTraining,accuracyValidation ]
forestResults ()
