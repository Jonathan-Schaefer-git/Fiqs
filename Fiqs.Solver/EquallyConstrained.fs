module EquallyConstrained

open MathNet.Numerics.LinearAlgebra
open Types


let solveEqualityConstrainedQP (p:QPProblem) =
    let Q = p.Q // n x n
    let E = p.A // n x m
    let d = p.b // m

    let c = p.c

    let n = Q.ColumnCount
    let m = E.RowCount

    let vec = Vector.Build.Dense(n + m)
    vec.SetSubVector(0,n, -c)
    vec.SetSubVector(n,m, d)

    // [ Q E^T \ E 0 ] * [ x \ lambda ] = [ -c \ d ]
    let matrix = Matrix.Build.Dense(n + m, n + m)
    matrix.SetSubMatrix(0,0,Q)
    matrix.SetSubMatrix(n,0,E.Transpose())
    matrix.SetSubMatrix(0,n,E)

    let chol = matrix.Cholesky()
    
    let sol = chol.Solve(vec)
    let x = sol.SubVector(0, n)

    let objVal = 0.5 * x * Q * x + c * x + p.constant

    Optimal {Result = x; ObjectiveValue = objVal; Iterations = 1}