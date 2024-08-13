module Solver

open System
open Xunit
open Fiqs
open MathNet.Numerics.LinearAlgebra
open Types

let tol = 1e-6

let validate (res:SolverResult) (expSol:Solution) =
    match res with
    | Optimal sol ->
        Assert.True((sol.Result - expSol.Result).L2Norm() < tol && sol.ObjectiveValue = expSol.ObjectiveValue && sol.Iterations <= expSol.Iterations)
    | Infeasible _ ->
        Assert.Fail()




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

    let result = QP.solveQP problem
    validate result expectedSol

[<Fact>]
let ``Test solveQP for a simple constrained QP problem`` () =
    // f(x) = (x-1)^2 = x^2 -2x + 1 s.t. x < 0
    let Q = matrix [[2.0]]
    let c = vector [-2.0]
    let A = matrix [[0.0]]
    let y = vector [0.0]
    let b = vector [0.0]
    let lowerBounds = vector [Double.NegativeInfinity]
    let upperBounds = vector [0.0]
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
        Result = vector [0.0]
        ObjectiveValue = 1
        Iterations = 100
    }

    let result = QP.solveQP problem
    validate result expectedSol
