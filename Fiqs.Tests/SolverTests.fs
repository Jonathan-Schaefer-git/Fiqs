module Solver

open System
open Xunit
open Fiqs.Types
open MathNet.Numerics.LinearAlgebra




[<Fact>]
let ``Test solveQP for a simple unconstrained QP problem`` () =
    // f(x) = (x-1)^2 = x^2 -2x + 1
    let x1 = { Name = DecisionName("x1"); Type = Continuous(0.0, 100.0) }

    
    let objective = (x1 * x1) + (3.0 * x1 + 5.0)
    let test = { Name = ObjectiveName "Minimize x"; Type = Minimize; Expression = objective }


    let constraint1 = x1 ==> 2.0

    let qp = { Objective = objective; Constraints = [constraint1]}

    let solver = AccordSolver.AccordSolver()

