module Solver

open System
open Xunit
open Fiqs
open MathNet.Numerics.LinearAlgebra


[<Fact>]
let ``Find unconstrained quadratic minimum`` () =
    let Q = Matrix<float>.Build.DenseOfArray (array2D [ [2.0;0.0]; [0.0;2.0] ])
    let c = Vector<float>.Build.DenseOfArray [|-2.0;-5.0|]
    let init = Vector<float>.Build.DenseOfArray [|0.0;0.0|]
    let sol = QP.findMinimum Q c init
    let expected = Vector<float>.Build.DenseOfArray [|1.0; 2.5|]
    Assert.Equivalent(sol, expected)
