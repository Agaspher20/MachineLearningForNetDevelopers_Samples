#I @"..\packages\"
#r @"Accord.3.4.0\lib\net45\Accord.dll"
#r @"Accord.MachineLearning.3.4.0\lib\net45\Accord.MachineLearning.dll"
#r @"Accord.Math.3.4.0\lib\net45\Accord.Math.dll"
#r @"Accord.Statistics.3.4.0\lib\net45\Accord.Statistics.dll"

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

let trainLogistic model (images:float[][]) (labels:float[]) =
    let learner = LogisticGradientDescent model
    learner.Learn(images, labels)

let trainedModel = trainLogistic model images labels

let accuracy (trainedModel:float[]->float) =
    validation
    |> Array.filter (fun (label,_) -> label = 4. || label = 9.)
    |> Array.map (fun (label,image) -> labeler label,image)
    |> Array.map (fun (label,image) ->
        let predicted = if trainedModel(image) > 0.5 then 1. else 0.
        let real = label
        if predicted = real then 1. else 0.)
    |> Array.average 
accuracy trainedModel.Score

let one_vs_all training =
    let features = 28 * 28
    let labels = [0.0 .. 9.0]
    let models =
        labels
        |> List.map (fun target -> 
            printfn "Learning label %.0f" target
            // create training set for target label
            let trainingLabels,trainingFeatures =
                training
                |> Array.map(fun (label, features) ->
                    if label = target
                    then (1.,features)
                    else (0.,features))
                |> Array.unzip
            // train the model
            let model = LogisticRegression()
            model.NumberOfInputs <- features
            let trainedModel = trainLogistic model trainingFeatures trainingLabels
            printfn "Learning finished for label %.0f" target
            target,trainedModel)
    let classifier (image:float[]) =
        models
        |> List.maxBy (fun (label, model) -> model.Score image)
        |> fun (label, confidence) -> label
    classifier

let trainedOneVsAll = one_vs_all training
accuracy trainedOneVsAll
