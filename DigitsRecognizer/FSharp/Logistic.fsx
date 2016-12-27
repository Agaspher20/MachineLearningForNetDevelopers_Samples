#I @"..\packages\"
#r @"Accord.3.3.0\lib\net45\Accord.dll"
#r @"Accord.MachineLearning.3.3.0\lib\net45\Accord.MachineLearning.dll"
#r @"Accord.Math.3.3.0\lib\net45\Accord.Math.dll"
#r @"Accord.Statistics.3.3.0\lib\net45\Accord.Statistics.dll"

open Accord.Statistics.Models.Regression
open Accord.Statistics.Models.Regression.Fitting

let readLogistic fileName =
    let path = __SOURCE_DIRECTORY__ + @"..\..\data\" + fileName

    path
    |> System.IO.File.ReadAllLines
    |> fun lines -> lines.[1..]
    |> Array.map (fun line ->
        let parsed = line.Split ',' |> Array.map float
        parsed.[0], parsed.[1..])

let training = readLogistic "trainingsample.csv"
let validation = readLogistic "validationsample.csv"

let labeler x =
    match x with
    | 4. -> 0.
    | 9. -> 1.
    | _ -> failwith "unexpected label"

let fours =
    training
    |> Array.filter (fun (label,_) -> label = 4.)
let nines =
    training
    |> Array.filter (fun (label,_) -> label = 9.)

let labels,images =
    Array.append fours nines
    |> Array.map (fun (label,image) -> labeler label,image)
    |> Array.unzip

let features = 28 * 28
let model = LogisticRegression()
model.NumberOfInputs <- features

let trainLogistic model =
    let learner = LogisticGradientDescent model

    learner.Learn(images, labels)

let trainedModel = trainLogistic model

let accuracy () =
    validation
    |> Array.filter (fun (label,_) -> label = 4. || label = 9.)
    |> Array.map (fun (label,image) -> labeler label,image)
    |> Array.map (fun (label,image) ->
        let predicted = if trainedModel.Score(image) > 0.5 then 1. else 0.
        let real = label
        if predicted = real then 1. else 0.)
    |> Array.average 
accuracy ()