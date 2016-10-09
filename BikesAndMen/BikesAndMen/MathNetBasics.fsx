#I @"..\packages\"
#r @"FSharp.Data.2.3.2\lib\net40\FSharp.Data.dll"
#load @"FSharp.Charting.0.90.14\FSharp.Charting.fsx"
#r @"MathNet.Numerics.Signed.3.13.1\lib\net40\MathNet.Numerics.dll"
#r @"MathNet.Numerics.FSharp.Signed.3.13.1\lib\net40\MathNet.Numerics.FSharp.dll"
 
open FSharp.Charting
open FSharp.Data
open MathNet
open MathNet.Numerics.LinearAlgebra
open MathNet.Numerics.LinearAlgebra.Double

let A = vector [1.; 2.; 3.]
let B = matrix [ [1.;2.]
                 [3.;4.]
                 [5.;6.]]
let C = A*A
let D = A*B
let E = A*B.Column(1)