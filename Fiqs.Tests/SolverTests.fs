module Solver

open System
open Xunit
open Fiqs
open MathNet.Numerics.LinearAlgebra
open Types

let tol = 1e-8

let validate (res:SolverResult) (expSol:Solution) =
    match res with
    | Optimal sol ->
        Assert.True((sol.Result - expSol.Result).L2Norm() < tol && sol.ObjectiveValue = expSol.ObjectiveValue && sol.Iterations <= expSol.Iterations)
    | Infeasible err ->
        Assert.Fail(err)




[<Fact>]
let ``Test solveQP for a simple unconstrained QP problem`` () =
    // f(x) = (x-1)^2 = x^2 -2x + 1
    let Q = matrix [[2.0]]
    let c = vector [-2.0]
    let A = matrix [[0.0]]
    let b = vector [0.0]
    let lowerBounds = vector [Double.NegativeInfinity]
    let upperBounds = vector [Double.PositiveInfinity]
    let tol = 1e-6
    
    let problem : QPProblem = {
        Q = Q
        c = c
        A = A
        b = b
        lowerBounds = lowerBounds
        upperBounds = upperBounds
        constant = 1.0
        tolerance = tol
        maxIterations = 1000
    }

    let expectedSol = {
        Result = vector [1.0]
        ObjectiveValue = 0
        Iterations = 100
    }

    let result = Barrier.solveQPBarrier problem
    validate result expectedSol

[<Fact>]
let ``Test solveQP with equality constraints`` () =
    // f(x) = (x-2)^2 + (y-1)^2 = x^2 + y^2 -4x -4y + 8
    let Q = matrix [[2.0; 0.0]; [0.0; 2.0]]
    let c = vector [-4.0; -4.0]
    let A = matrix [[-1.0;1.0]; [1.0;4.0]; [1.0; 1.0]]
    let b = vector [-2.0;8.0; 3.0]
    let lowerBounds = vector [Double.NegativeInfinity; Double.NegativeInfinity]
    let upperBounds = vector [Double.PositiveInfinity; Double.PositiveInfinity]

    let problem : QPProblem = {
        Q = Q
        c = c
        A = A
        b = b
        lowerBounds = lowerBounds
        upperBounds = upperBounds
        constant = 8.0
        tolerance = tol
        maxIterations = 1000
    }

    let expectedSol = {
        Result = vector [1.0; 1.0]
        ObjectiveValue = 0.5
        Iterations = 100
    }

    let result = Barrier.solveQPBarrier problem
    validate result expectedSol
