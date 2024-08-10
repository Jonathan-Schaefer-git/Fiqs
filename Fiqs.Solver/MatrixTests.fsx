#r "nuget:MathNet.Numerics.FSharp"
open MathNet.Numerics.LinearAlgebra

let Q = matrix [[1.0;4.0];[4.0;0.0]]
let x = vector [1.0;1.0]
Q * x

let A = matrix [[1.0; 1.0]]
let At = A.Transpose()
let lambda = vector [4.0]
At * lambda

let c = vector [7.0;2.0]

Q * x - At * lambda + c
