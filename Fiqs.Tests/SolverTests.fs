module Solver

open System
open Xunit
open Fiqs
open MathNet.Numerics.LinearAlgebra
open Types



[<Fact>]
let ``Test solveQP for an unconstrained QP problem`` () =
    // f(x) = (x-1)^2 = x^2 -2x + 1
    let Q = matrix [[2.0]]
    let c = vector [-2.0]
    let A = matrix [[0.0]]
    let y = vector [0.0]
    let b = vector [0.0]
    let lowerBounds = vector [Double.NegativeInfinity]
    let upperBounds = vector [Double.PositiveInfinity]
    let tol = 1e-6
    
    let problem : QPProblem = {
        Q = Q
        c = c
        A = A
        y = y
        b = b
        lowerBounds = lowerBounds
        upperBounds = upperBounds
        constant = 1.0
        tolerance = tol
    }

    let expectedSol = {
        Result = vector [1.0]
        ObjectiveValue = 0
        Iterations = 1
    }
    // Solve the QP problem
    let result = QP.solveQP problem

    // Validate the solution
    match result with
    | Optimal sol ->
        Assert.True (sol.Result = expectedSol.Result && sol.ObjectiveValue = expectedSol.ObjectiveValue && sol.Iterations <= expectedSol.Iterations)
    | Infeasible _ ->
        Assert.Fail("Model is infeasible")
