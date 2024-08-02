module Solver

open System
open Xunit
open Fiqs
open Newton
open MathNet.Numerics.LinearAlgebra
[<Fact>]
let ``Newton method`` () =

    let Q = Matrix<float>.Build.DenseOfArray (array2D [ [2;0]; [0;2]])
    let c = Vector<float>.Build.DenseOfArray [|-2;-5|]
    let init = Vector<float>.Build.DenseOfArray [|0;0|]
    let tol = 1e-6
    let sol = findMinimum Q c tol init 1000
    printfn "%A" (sol.Storage.ToArray())
    Assert.NotNull(sol.Storage)
