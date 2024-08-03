module QP
open Types
open MathNet.Numerics.LinearAlgebra
open MathNet.Numerics.Optimization

// 1.Newton Schritt für Polynome zweiten Grades immer Minimum
let findMinimum (Q:Matrix<float>) (c:Vector<float>) (initGuess: Vector<float>) : Vector<float> =
    initGuess - Q.Inverse() * c


let solveQP (p:QPProblem) : (Vector<float> * float) option =
    let Q = p.Q
    let c = p.c
    let A = p.A
    let b = p.b
    let lB = p.lowerBounds
    let uB = p.upperBounds

    let objective (x:Vector<float>) : float =
        0.5 * ((Q * x) * x) + (c * x)
        
    let gradient (x:Vector<float>) : Vector<float> =
        Q * x + c

    let constraints (x:Vector<float>) : Vector<float> =
        A * x - b

    let barrierTerm (x: Vector<float>) (lowerBounds: Vector<float>) (upperBounds: Vector<float>) : float =
        let lowerTerm = x - lowerBounds
        let upperTerm = upperBounds - x
        - (lowerTerm.Map(fun xi -> log xi) + upperTerm.Map(fun xi -> log xi)).Sum()
    
    let barrierGradient (x: Vector<float>) (lowerBounds: Vector<float>) (upperBounds: Vector<float>) : Vector<float> =
        let lowerTerm = x - lowerBounds
        let upperTerm = upperBounds - x
        lowerTerm.Map(fun xi -> 1.0 / xi) - upperTerm.Map(fun xi -> 1.0 / xi)
    


    let bounds =
        [| 
            for i in [0..(lB.Count - 1)] -> 
                (lB.[i], uB.[i])
        |]

    Some (Vector<float>.Build.DenseOfArray [|0;1|], 1.0)
        
    

