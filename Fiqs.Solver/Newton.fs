module Newton
open Types
open MathNet.Numerics.LinearAlgebra

// Using Newtons method to find the local minimum
let findMinimum (Q:Matrix<float>) (c:Vector<float>) (tol:float) (initGuess: Vector<float>) (maxIterations:int) : Vector<float> =


    let rec iterate (x:Vector<float>) i : Vector<float> =
        match i > maxIterations with
        | true -> x
        | false ->
            let gradient = Q * x + c
            let hessian = Q

            let step = hessian.Inverse() * gradient
            if step.L2Norm() < tol then
                x
            else
                iterate (x - step) (i + 1)

    iterate initGuess 0