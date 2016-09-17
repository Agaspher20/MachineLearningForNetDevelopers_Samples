open System.IO

type Observation = { Label:string; Pixels: int[] }

let toObservation (csvData:string) =
    let columns = csvData.Split(',')
    {
        Label = columns.[0];
        Pixels = columns.[1..] |> Array.map int
    }

let reader path =
    let data = File.ReadAllLines path
    data.[1..] |> Array.map toObservation

let generalPath = __SOURCE_DIRECTORY__ + @"..\..\data\";

let trainingPath = generalPath + "trainingsample.csv"
let trainingData = reader trainingPath

type Distance = int[] * int[] -> int

let manhattanDistance (pixels1, pixels2) =
    Array.zip pixels1 pixels2
    |> Array.map (fun (x,y) -> abs (x-y))
    |> Array.sum

let euclideanDistance (pixels1, pixels2) =
    Array.zip pixels1 pixels2
    |> Array.map (fun (x,y) ->
        let z = x-y
        z*z)
    |> Array.sum

let train (trainingSet:Observation[]) (dist:Distance) =
    let classify (pixels:int[]) =
        trainingSet
        |> Array.minBy (fun x -> dist(x.Pixels, pixels))
        |> fun x -> x.Label
    classify

type Classifier = int[] -> string
 
let manhattanClassifier = train trainingData manhattanDistance
let euclideanClassifier = train trainingData euclideanDistance

let validationPath = generalPath + "validationsample.csv";

let validationData = reader validationPath

let evaluate (data:Observation[]) (classifier:Classifier) =
    data
    |> Array.averageBy (fun x -> if classifier x.Pixels = x.Label then 1. else 0.)
    |> fun x -> x * 100.0

evaluate validationData manhattanClassifier |> printfn "Manhattan: %.3f"
evaluate validationData euclideanClassifier |> printfn "Euclidean: %.3f"
