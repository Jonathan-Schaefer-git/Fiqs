module Solver

open System
open Xunit
open Fiqs
open MathNet.Numerics.LinearAlgebra
open Types


[<Fact>]
let ``Test solveQP with a simple QP problem`` () =
    // Define a simple QP problem
    let Q = matrix [[4.0; 1.0]; [1.0; 2.0]]
    let c = vector [-1.0; -1.0]
    let A = matrix [[1.0; 1.0]]
    let y = vector [1.0]
    let b = vector [1.0]
    let lowerBounds = vector [0.0; 0.0]
    let upperBounds = vector [10.0; 10.0]
    let tol = 1e-6
    
    let problem : QPProblem = {
        Q = Q
        c = c
        A = A
        y = y
        b = b
        lowerBounds = lowerBounds
        upperBounds = upperBounds
        tolerance = tol
    }
    
    // Solve the QP problem
    let solution = QP.solveQP problem

    // Expected solution (for this particular QP problem)
    let expected_x = vector [0.0; 1.0] // This should be replaced with the actual expected solution
    let expected_iterations = 1 // This depends on your implementation details
    
    // Validate the solution
    match solution with
    | Some (x, iterations) ->
        Assert.True((x - expected_x).L2Norm() < tol, "Solution does not match the expected solution.")
        Assert.Equal(expected_iterations, iterations)
    | None ->
        Assert.True(false, "Solver failed to find a solution.")
