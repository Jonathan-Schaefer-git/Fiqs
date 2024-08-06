module QP
open Types
open MathNet.Numerics.LinearAlgebra
open MathNet.Numerics.Optimization

let solveQP (p:QPProblem) : (Vector<float> * float) option =
    let Q = p.Q
    let c = p.c
    let A = p.A
    let At = A.Transpose()
    let b = p.b
    let lB = p.lowerBounds
    let uB = p.upperBounds


    let n = Q.RowCount
    let m = A.ColumnCount
    
    let x = Vector<float>.Build.Dense(n, 1)
    let lamba = Vector<float>.Build.Dense(m, 1)
    let e = Vector<float>.Build.Dense(m, 1)

    let tol = 1e-6
    let maxIterations = 100

    // Predictor Corrector Verfahren
    let rec iterate (x:Vector<float>) (y:Vector<float>) (lambda:Vector<float>) (r_d:Vector<float>) (r_p:Vector<float>) i : Vector<float> option =
        match i >= maxIterations with
        | true -> None
        | false ->
            // Solve KKT and obtain affine direction
            let Y = Matrix<float>.Build.SparseOfDiagonalVector y
            let Yi = Y.Transpose()
            let Lambda = Matrix.Build.SparseOfDiagonalVector lambda
            let LambdaI = Lambda.Inverse()

            let r_d = Q * x - At * lambda + c
            let r_p = A * x - y - b
            let mu = (y * lambda) / (float m)

            let M = Q + At * Lambda * Yi * A

            let calcAffinityGrad (sigma:float) = 
                -r_d + (At * Yi * r_p) + (At * Yi * e) + (At * Yi * -sigma * mu * e)

            let a = calcAffinityGrad 0.0

            let affine = M.Solve(a)




    iterate x p.y lamba 


        
    

