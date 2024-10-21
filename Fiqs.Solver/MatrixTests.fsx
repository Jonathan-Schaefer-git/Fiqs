#r "nuget:MathNet.Numerics.FSharp"
open MathNet.Numerics.LinearAlgebra
open System


let projectedGradientDescent (Q: Matrix<float>) (c: Vector<float>) (A: Matrix<float>) (b: Vector<float>) (alpha: float) (maxIter: int) (tol: float) =
    // Objective function: 0.5 * x'Qx + c'x
    let f (x: Vector<float>) = 0.5 * x * Q * x + c * x
    // Gradient of the objective function: Qx + c
    let gradient (x: Vector<float>) = Q * x + c

    // Projection of x onto the feasible region defined by Ax <= b
    let project (x: Vector<float>) =
        // Simple bound constraints example: clip values between bounds
        // This is where you'd implement the more complex projection logic
        x.Map(fun xi -> max 0.0 (min xi 1.0))  // Example: projecting between 0 and 1

    // Initialize starting point (make sure it's feasible, i.e., Ax <= b)
    let mutable x = vector [|0.0; 0.0|]  // Example 2D problem

    let mutable iter = 0
    let mutable diff = Double.MaxValue

    // Main optimization loop
    while iter < maxIter && diff > tol do
        let g = gradient x
        let xTemp = x - alpha * g

        // Project the solution back to the feasible set
        let xNext = project xTemp

        // Check convergence
        diff <- (xNext - x).L2Norm()

        // Update for next iteration
        x <- xNext
        iter <- iter + 1

    // Return final solution
    x

// Example use:
let Q = matrix [| [|2.0; 0.0|]; [|0.0; 2.0|] |]
let c = vector [|-2.0; -2.0|]
let A = matrix [| [|1.0; 1.0|] |]
let b = vector [|1.0|]

let solution = projectedGradientDescent Q c A b 0.1 1000 1e-6
printfn "Solution: %A" solution