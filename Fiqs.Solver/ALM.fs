module ALM

open MathNet.Numerics.LinearAlgebra

// Quadratic problem definition: Minimize 1/2 x^T Q x + c^T x subject to Ax <= b
let Q = matrix [| [|2.0; 0.0|]; [|0.0; 2.0|] |]  // Positive semi-definite matrix
let c = vector [| -2.0; -5.0 |]                   // Objective function vector
let A = matrix [| [|1.0; 2.0|]; [|4.0; 1.0|] |]   // Inequality constraint matrix
let b = vector [| 3.0; 6.0 |]                     // Inequality constraint vector

// Augmented Lagrangian parameters
let rho = 1.0  // Penalty parameter
let maxIterations = 100
let tolerance = 1e-6

// Max(0, x) operation element-wise
let maxZero (v: Vector<float>) =
    v.Map(fun x -> max 0.0 x)
    

// Augmented Lagrangian function
let augmentedLagrangian (x: Vector<float>) (lambda: Vector<float>) =
    let lagrangian = 
        0.5 * x * Q * x + c * x + lambda * A * x - b
    let penalty = 0.5 * rho * (maxZero (A * x - b)).DotProduct(maxZero (A * x - b))
    lagrangian + penalty

// Gradient of the augmented Lagrangian
let gradient (x: Vector<float>) (lambda: Vector<float>) =
    Q * x + c + A.Transpose() * (lambda + rho * maxZero (A * x - b))

// Solve unconstrained minimization using gradient descent (simplified)
let gradientDescent (x0: Vector<float>) (lambda: Vector<float>) =
    let alpha = 0.01  // Step size
    let rec iterate (x:Vector<float>) (i:int) =
        if i > maxIterations then
            failwith "Max iterations reached within gradient descent"
        
        let grad = gradient x lambda
        match grad.L2Norm() < tolerance with
        | true -> x
        | false ->
            let x_step = x - alpha * grad
            iterate x_step (i + 1)
    iterate x0 0

// Augmented Lagrangian method
let augmentedLagrangianMethod (x0: Vector<float>) =

    let lambda_0 = DenseVector.zero b.Count  // Initialize Lagrange multipliers
    let rec iterate (x:Vector<float>) (lambda:Vector<float>) (i:int) =
        let constraintViolation = A * x - b
        match constraintViolation.L2Norm() < tolerance with
        | true -> x
        | false ->
            let x_desc = gradientDescent x lambda
            let lambda_desc = lambda + rho * maxZero constraintViolation
            iterate x_desc lambda_desc (i + 1)

    iterate x0 lambda_0 0
// Initial guess for x
let x0 = vector [| 0.0; 0.0 |]

// Run the ALM solver
let solution = augmentedLagrangianMethod x0

// Print the solution
printfn "Solution: %A" solution
