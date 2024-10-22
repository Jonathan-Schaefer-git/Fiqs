open MathNet.Numerics.LinearAlgebra
let Q = matrix [[ 2.0; 0.0 ]; [ 0.0; 2.0 ]]
let c = vector [ 2.0; 2.0 ]
let A = matrix [[ 2.0; 1.0 ]]
let b = vector [ 1.0 ]
let At = A.Transpose()

let tol = 1e-6
let maxIter = 100000


let augmentedLagrangian (x: Vector<float>) (lambda: Vector<float>) (r: float) =
    let penaltyTerm = 0.5 * r * ((A * x - b).L2Norm() ** 2.0)
    let lagrangeMultiplierTerm = lambda * (A * x - b)
    0.5 * x * Q * x + c * x + lagrangeMultiplierTerm + penaltyTerm

let updateLagrangeMultipliers (lambda: Vector<float>) (x: Vector<float>) (r: float) =
    lambda + r * (b - A * x)

let gradientAugmentedLagrangian (x: Vector<float>) (lambda: Vector<float>) (r: float) =
    let lagrangeTerm = A.Transpose() * lambda
    let penaltyTerm = A.Transpose() * (A * x - b) * r
    Q * x + c - lagrangeTerm - penaltyTerm

let minimizePrimal (x: Vector<float>) (lambda: Vector<float>) (r: float) (tol: float) (maxIter: int) =
    // Initialize gradient and search direction
    let mutable g = gradientAugmentedLagrangian x lambda r
    let mutable d = -g
    let mutable x_current = x
    let mutable iter = 0

    while g.L2Norm() > tol && iter < maxIter do
        // Compute step size (exact minimization for QP)
        let alpha = -(g.DotProduct d) / (d.DotProduct (Q * d))

        // Update x and gradient
        x_current <- x_current + alpha * d
        let g_new = gradientAugmentedLagrangian x_current lambda r

        // Polak-Ribiere update for beta
        let beta = (g_new.DotProduct (g_new - g)) / (g.DotProduct g)

        // Update search direction
        d <- -g_new + beta * d

        // Update gradient and iteration count
        g <- g_new
        iter <- iter + 1

    // Return the optimized primal variable
    x_current


let rec augmentedLagrangianMethod (x: Vector<float>) (lambda: Vector<float>) (r: float) (tol: float) (iter: int) =
    if iter > 100000 then failwith "Max iterations reached"
    else
        // Solve the primal problem (minimize Lagrangian over x)
        let primalObjective = fun (x: Vector<float>) -> augmentedLagrangian x lambda r
        // Use a solver like nonlinear CG or another minimizer to minimize primalObjective
        let new_x : Vector<float> = minimizePrimal x lambda r tol 100

        // Check constraint satisfaction
        let constraintViolation = (A * new_x - b).L2Norm()
        if constraintViolation < tol then
            new_x, lambda // Converged
        else
            // Update Lagrange multipliers
            let new_lambda = updateLagrangeMultipliers lambda new_x r
            // Update penalty parameter r if needed (e.g., r *= 1.1)
            let new_r = r * 1.1
            // Continue iterating
            augmentedLagrangianMethod new_x new_lambda new_r tol (iter + 1)


printfn "%A" (augmentedLagrangianMethod (vector [ 1.0; 1.0 ]) (vector [1.0]) 1.0 tol 0)
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
