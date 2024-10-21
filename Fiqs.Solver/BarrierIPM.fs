module Barrier
open System

open Types
open MathNet.Numerics.LinearAlgebra
open MathNet.Numerics.LinearAlgebra.Factorization



let solveQPBarrier(problem:QPProblem) =
    let Q = problem.Q
    let c = problem.c
    let A = problem.A
    let At = A.Transpose()
    let b = problem.b
    let lB = problem.lowerBounds
    let uB = problem.upperBounds
    let epsilon = problem.tolerance
    let maxIterations = problem.maxIterations

    let n = Q.RowCount
    let m = A.RowCount

    let e = Vector<float>.Build.Dense(n, 1)

    let rec iterate (x:Vector<float>) () (i:int)