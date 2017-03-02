#I @"..\packages\"
#r @"Accord.3.4.0\lib\net45\Accord.dll"
#r @"Accord.MachineLearning.3.4.0\lib\net45\Accord.MachineLearning.dll"
#r @"Accord.Math.3.4.0\lib\net45\Accord.Math.dll"
#r @"Accord.Statistics.3.4.0\lib\net45\Accord.Statistics.dll"

open Accord.Statistics.Models.Regression
open Accord.Statistics.Models.Regression.Fitting

let svmRead fileName =
    let path = __SOURCE_DIRECTORY__ + @"..\..\data\" + fileName
    path
    |> System.IO.File.ReadAllLines
    |> fun lines -> lines.[1..]
    |> Array.map (fun line ->
        let parsed = line.Split ','
        parsed.[0] |> int, parsed.[1..] |> Array.map float)
let labels,images = svmRead "trainingsample.csv" |> Array.unzip 

open Accord.MachineLearning
open Accord.MachineLearning.VectorMachines
open Accord.MachineLearning.VectorMachines.Learning
open Accord.Statistics.Kernels 

let features = 28 * 28
let classes = 10

let teacher = new MulticlassSupportVectorLearning<Linear>()
teacher.Learner <- fun parameters ->
    let learner = SequentialMinimalOptimization<Linear>()
    learner :> ISupervisedLearning<SupportVectorMachine<Linear>, float[], bool>
teacher.ParallelOptions.MaxDegreeOfParallelism <- 1
let machine = teacher.Learn(images, labels)

let calibration = MulticlassSupportVectorLearning<Linear>()
calibration.Model <- machine
calibration.Learner <- fun parameters ->
    let calibr = ProbabilisticOutputCalibration<Linear>()
    calibr.Model <- parameters.Model
    calibr :> ISupervisedLearning<SupportVectorMachine<Linear>, float[], bool>

calibration.ParallelOptions.MaxDegreeOfParallelism = 1;

calibration.Learn(images, labels);

let validation = svmRead "validationsample.csv"
validation |> Array.averageBy (fun (l,i) -> if machine.Decide i = l then 1. else 0.)
