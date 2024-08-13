module QP
open Types
open MathNet.Numerics.LinearAlgebra
open MathNet.Numerics.LinearAlgebra.Factorization
open MathNet.Numerics.Optimization

let solveQP (p:QPProblem) : SolverResult =
    let Q = p.Q
    let c = p.c
    let A = p.A
    let At = A.Transpose()
    let b = p.b
    let lB = p.lowerBounds
    let uB = p.upperBounds


    let n = Q.RowCount
    let m = A.RowCount
    
    let x = Vector<float>.Build.Dense(n, 1)
    let lambda = Vector<float>.Build.Dense(m, 1)
    let y = Vector<float>.Build.Dense(m, 1)

    let e = Vector<float>.Build.Dense(m, 1)

    let tol = 1e-6
    let maxIterations = 1000

    // Predictor Corrector Verfahren
    let rec iterate (x:Vector<float>) (lambda:Vector<float>) (y:Vector<float>) (tau:float) (i:int) : Solution option =
        
        // Residuen
        let r_d = (Q * x) - (At * lambda) + c
        let r_p = (A * x) - y - b

        if r_d.L2Norm() < tol && r_p.L2Norm() < tol then
            let optimalSol = 0.5 * x * Q * x + c * x + p.constant
            Some {Result = x; ObjectiveValue = optimalSol; Iterations = i}

        elif i >= maxIterations then
            None

        else
            // KKT lösen und affine Richtung finden
            let Y = Matrix<float>.Build.SparseOfDiagonalVector y
            let Yi = Y.Transpose()
            let Lambda = Matrix.Build.SparseOfDiagonalVector lambda
            let LambdaI = Lambda.Inverse()

            let mu = (y * lambda) / (float m)

            // Predictor Schritt
            let r_c_aff = 
                (Y * Lambda * e)

            let M = Q + At * Lambda * Yi * A
            // Der Term -(sigma * mu * e) kann ignoriert werden, da er null für die Bestimmung der affinen Richtung ist
            let b_aff = -r_d + (At * Yi * Lambda * r_p) + (At * Lambda * e)

            // Cholesky-Zerlegung; Wird auch im Corrector Schritt wiederverwendet
            let M_chol = M.Cholesky()
                        
            let x_aff = M_chol.Solve(b_aff)
            let lambda_aff = -Yi * (Lambda * (r_p + A * x_aff)) - e
            let y_aff = -LambdaI * (r_c_aff + Y * lambda_aff)

            // Corrector Schritt
            let Y_aff = Matrix<float>.Build.SparseOfDiagonalVector y_aff
            let Lambda_aff = Matrix<float>.Build.SparseOfDiagonalVector lambda_aff

            // Zentrierungsparameter finden
            let mu_aff = (y_aff * lambda_aff) / (float m)
            let sigma = pown (mu_aff / mu) 3
            
            let r_c_corr =
                (Lambda * Y * e) + (Lambda_aff * Y_aff * e) - (sigma * mu * e)
            let b_corr =
                (-r_d) + At * Yi * ((Lambda * r_p) + r_c_corr)
            
            let x_corr = M_chol.Solve(b_corr)
            let lambda_corr = -Yi * (r_p * Lambda + r_c_corr + (Lambda * A * x_corr))
            let y_corr = (-r_c_corr * LambdaI) - (Y * LambdaI * lambda_corr)

            let delta_x = x_aff + x_corr
            let delta_lambda = lambda_aff + lambda_corr
            let delta_y = y_aff + y_corr


            // Gleichheitsschrittweite
            let alpha_primary = 
                Seq.zip (y) (delta_y)
                |> Seq.map(fun (y_i, d_y) -> 
                    if d_y < 0.0 then (1.0 - tau) * y_i / -d_y else 1.0)
                |> Seq.min

            let alpha_dual = 
                Seq.zip (lambda) (delta_lambda)
                |> Seq.map(fun (lambda_i, delta_lambda_i) -> 
                    if delta_lambda_i < 0.0 then (1.0 - tau) * lambda_i / -delta_lambda_i else 1.0)
                |> Seq.min
                 
            let alpha = min alpha_primary alpha_dual

            let new_x = x + alpha * delta_x
            let new_lambda = lambda + alpha * delta_lambda
            let new_y = y + alpha * delta_y

            // This applies the boundries element-wise
            let projected_x =
                new_x.MapIndexed(fun i x_i -> (max (min x_i uB.[i]) lB.[i]))


            iterate projected_x new_lambda new_y (1.0 - (1.0 - tau) * exp(-0.2 * float i)) (i + 1)
    match iterate x lambda y 0.0 0 with
    | Some sol -> Optimal sol
    | None -> Infeasible "The model was deemed to be infeasible as it couldn't be solved in an appropriate time"