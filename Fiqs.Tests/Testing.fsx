#r "nuget:MathNet.Numerics.FSharp"
open MathNet.Numerics.LinearAlgebra

let a = Matrix<float>.Build.DenseOfArray (array2D [ [1;5]; [5; 7]])
let b = Vector<float>.Build.DenseOfArray [| 3; 2 |]
let c = a * b
