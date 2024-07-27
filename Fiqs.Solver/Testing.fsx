#r "nuget:MathNet.Numerics"


// Active set method
open MathNet.Numerics.LinearAlgebra
open MathNet.Numerics.LinearAlgebra.Double

let A = 
    array2D [
        [3.0;2.0;-1.0]
        [2.0;-2.0;4.0]
        [-1.0;0.5;-1.0]
    ]
    |> Matrix<float>.Build.DenseOfArray


let b = Vector<float>.Build.Dense([|1.0;-2;0|])
let sol = A.Solve(b)
sol.Storage



// Branch and cut