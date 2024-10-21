open MathNet.Numerics.LinearAlgebra


let Q = matrix [ [ 2.0; 0 ]; [ 0; 2 ] ]
let c = vector [ 2.0; 2 ]
let e = vector [ 1.0 ; 1 ]
let lb = vector [ 0.0; 0 ]
let ub = vector [ 3.0; 3 ]


let tol = 1e-6
[<TailCall>]
let rec simpleNewtons (x:Vector<float>) (mu:float) (i:int) =
    match i > 1000 with
    | true -> failwith "Reached max iterations"
    | false ->
        let X = DiagonalMatrix.ofDiag x
        let Xi = X.Inverse()

        let gradient = Q * x + c + (Xi * e)
        if gradient.L2Norm() < tol then
            x
        else
            let delta_x = (Q - X).Solve(-1.0 * gradient)
            let new_x = x + delta_x
            simpleNewtons new_x (i + 1)

printfn "%A" (simpleNewtons (vector [2.0; 2.0]) 0)