open System.IO

type Observation = { Label:string; Pixels: int[] }
type Distance = int[] * int[] -> int
type Classifier = int[] -> string

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

let manhattanDistance (pixels1, pixels2) =
    Array.zip pixels1 pixels2
    |> Array.map (fun (x,y) -> abs (x-y))
    |> Array.sum

let euclideanDistance (pixels1:int[], pixels2:int[]) =
    let dim = pixels1.Length
    let mutable dist = 0
    for i in 0 .. (dim - 1) do
        let x = pixels1.[i] - pixels2.[i]
        dist <- dist + (x * x)
    dist

let train (trainingSet:Observation[]) (dist:Distance) =
    let classify (pixels:int[]) =
        trainingSet
        |> Array.minBy (fun x -> dist(x.Pixels, pixels))
        |> fun x -> x.Label
    classify
 
let manhattanClassifier = train trainingData manhattanDistance
let euclideanClassifier = train trainingData euclideanDistance

let validationPath = generalPath + "validationsample.csv";

let validationData = reader validationPath

let evaluate (data:Observation[]) (classifier:Classifier) =
    data
    |> Array.averageBy (fun x -> if classifier x.Pixels = x.Label then 100. else 0.)

let parallelEvaluate validationSet classifier =
    validationSet
    |> Array.Parallel.map (fun x -> if classifier x.Pixels = x.Label then 100. else 0.)
    |> Array.average

evaluate validationData manhattanClassifier |> printfn "Manhattan: %.3f"

#time "on"
parallelEvaluate validationData euclideanClassifier |> printfn "Euclidean: %.3f"

let img1 = trainingData.[0].Pixels
let img2 = trainingData.[2].Pixels

for i in 1 .. 5000 do
    let dist = euclideanDistance(img1, img2)
    ignore()