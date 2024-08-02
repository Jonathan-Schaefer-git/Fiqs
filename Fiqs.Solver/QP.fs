module QP
open Types
open MathNet.Numerics.LinearAlgebra
open MathNet.Numerics.Optimization

let solveQP (p:QPProblem) : (Vector<float> * float) option =
    let Q = p.Q
    let c = p.c
    let A = p.A
    let b = p.b
    let lB = p.lowerBounds
    let uB = p.upperBounds

    //let objective (x:Vector<float>) : float =
    //    0.5 * (x.ToRowMatrix() * (Q * x)) + (c * x)
        

    let gradient (x:Vector<float>) : Vector<float> =
        Q * x + c

    let constraints (x:Vector<float>) : Vector<float> =
        A * x - b

    let bounds =
        [| 
            for i in [0..(lB.Count - 1)] -> 
                (lB.[i], uB.[i])
        |]

    Some (Vector<float>.Build.DenseOfArray [|0;1|], 1.0)
        
    

