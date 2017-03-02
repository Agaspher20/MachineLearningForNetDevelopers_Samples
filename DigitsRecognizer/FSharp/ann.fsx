#I @"..\packages\"
#r @"Accord.3.4.0\lib\net45\Accord.dll"
#r @"Accord.Math.3.4.0\lib\net45\Accord.Math.dll"
#r @"Accord.Neuro.3.4.0\lib\net45\Accord.Neuro.dll"
#r @"Accord.Statistics.3.4.0\lib\net45\Accord.Statistics.dll"

open Accord.Statistics
open Accord.Math
open Accord.Neuro
open Accord.Neuro.Learning

let nnRead fileName =
    let path = __SOURCE_DIRECTORY__ + @"..\..\data\" + fileName
    path
    |> System.IO.File.ReadAllLines
    |> fun lines -> lines.[1..]
    |> Array.map (fun line ->
        let parsed = line.Split ','
        parsed.[0] |> int, parsed.[1..] |> Array.map float)

let trainNetwork (epochs:int) =
    let features = 28*28
    let labels,images = nnRead "trainingsample.csv" |> Array.unzip
    let learningLabels = Jagged.OneHot(labels)
    let network = ActivationNetwork(SigmoidFunction(), features, [| 100; 10 |])

    NguyenWidrow(network).Randomize()

    let teacher = new ParallelResilientBackpropagationLearning(network)
    let rec learn iter =
        let error = teacher.RunEpoch(images, learningLabels)
        printfn "%.3f / %i" error iter
        if error < 0.01 then ignore()
        elif iter > epochs then ignore()
        else learn (iter + 1)

    learn 0

    network

let ann = trainNetwork(50)
let validate = nnRead "validationSample.csv"

validate
|> Array.averageBy (fun (label, image) ->
    let predicted =
        ann.Compute image
        |> Array.mapi (fun i x -> i,x)
        |> Array.maxBy snd
        |> fst
    if label = predicted then 1.0 else 0.0)
