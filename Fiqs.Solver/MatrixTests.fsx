#r "nuget:MathNet.Numerics.FSharp"
#r "nuget:Flips"
#r "nuget:Accord"
#r "nuget:Accord.Math"
open MathNet.Numerics.LinearAlgebra
open System
open System.Collections.Generic
open Flips
open Accord.Math.Optimization

// x^2 + y^2 + x + y
let Q = matrix [ [ 2.0; 0.0 ]; [ 0.0; 2.0 ] ]
let c = vector [ 1.0; 1.0 ]

let A = matrix [ [ 1.0; 1.0 ]]
let b = vector [ 1.0 ]

let obj = QuadraticObjectiveFunction(Q.ToArray(), c.ToArray())
let rows = A.ToRowArrays()
let constraints : IEnumerable<IConstraint> = 
    rows 
    |> Array.mapi (
        fun i A ->
            let constr = LinearConstraint(A)
            constr.Value <- b.[i]
            constr.ShouldBe <- ConstraintType.GreaterThanOrEqualTo
            constr
        ) 
    |> Seq.ofArray
    |> Seq.map(fun x -> x)



let alm = AugmentedLagrangian(obj, constraints)
alm.Minimize()
printfn "%A" alm.Optimizer.Solution
