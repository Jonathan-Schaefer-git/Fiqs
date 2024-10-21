open MathNet.Numerics.LinearAlgebra
let Q = matrix [[ 2.0; 0.0 ]; [ 0.0; 2.0 ]]
let c = vector [ 2.0; 2.0 ]
let A = matrix [[ 2.0; 1.0 ]]
let b = vector [ 1.0 ]
let At = A.Transpose()

let tol = 1e-6
let maxIter = 1000
let muInit = 2.0

let rec nonlinearConjugateGradient (x: Vector<float>) (g: Vector<float>) (d: Vector<float>) (i: int) =
    if i > maxIter then
        failwith "Reached max iterations"
    elif g.L2Norm() < tol then
        x, i // Converged
    else
        // Step size (line search or exact minimization for QP)
        let alpha = - (g * d) / (d.DotProduct (Q * d))
        
        // Update x and gradient
        let new_x = x + alpha * d
        let new_g = Q * new_x + c
        
        // Polak-Ribiere update for beta
        let beta = (new_g.DotProduct (new_g - g)) / (g.DotProduct g)
        
        // Update direction
        let new_d = -new_g + beta * d
        
        // Recur with updated values
        nonlinearConjugateGradient new_x new_g new_d (i + 1)


let x0 = vector [100.0; 100.0] // Example starting point
let g0 = Q * x0 + c // Gradient at x0
let d0 = -g0 // Initial direction is negative gradient

let solution = nonlinearConjugateGradient x0 g0 d0 0
printfn "Solution: %A" solution

//let tol = 1e-6
//let smallValueThreshold = 1e-8
//[<TailCall>]
//let rec barrierNewton (x:Vector<float>) (mu:float) (i:int) =
//    match i > 100000 with
//    | true -> failwith "Reached max iterations"
//    | false ->
//        let boundsGrad = -1.0 / (x - lB) + -1.0 / (uB - x)
//        let inequalityGrad = At * (1.0 / (A * x - b)).Map(fun v -> max v smallValueThreshold)
//        let gradient = Q * x + c + mu * (boundsGrad + inequalityGrad)
        
//        let res_lB = (x - lB).Map(fun x_i -> max (x_i ** 2) smallValueThreshold)
//        let res_uB = (uB - x).Map(fun x_i -> max (x_i ** 2) smallValueThreshold)
//        let res_Ax = (A * x - b).Map(fun x_i -> max (x_i ** 2) smallValueThreshold)

//        let inequality_constraints = mu * At * DiagonalMatrix.ofDiag(1.0 / (res_Ax)) * A
//        let bounds = DiagonalMatrix.ofDiag(1.0 / res_lB) + DiagonalMatrix.ofDiag(1.0 / res_uB)
//        let hessian = Q + mu * (bounds + inequality_constraints)
//        let delta_x = hessian.Solve(-gradient)
//        let new_x = x + mu * delta_x
//        if delta_x.L2Norm() < tol || mu < tol then
//            printfn "%A; %A; Mu %A; L2Norm %A;" (delta_x) (new_x) (mu) (delta_x.L2Norm())
//            x, i
//        else
//            printfn "%A; %A; Mu %A; L2Norm %A;" (delta_x) (new_x) (mu) (delta_x.L2Norm())
//            barrierNewton new_x (mu - mu * 0.2) (i + 1)


//printfn "%A" (barrierNewton (vector [1.0; 1.0]) 0.8 0)
