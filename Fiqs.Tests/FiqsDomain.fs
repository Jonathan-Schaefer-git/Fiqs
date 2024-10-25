module FiqsDomain

open Xunit
open Fiqs.Types
open Fiqs


[<Fact>]
let ``Test solver for a simple unconstrained QP problem`` () =
    // f(x) = (x-1)^2 = x^2 -2x + 1
    let x1 = { Name = DecisionName("x1"); Type = Continuous(0.0, 100.0) }

    let objective = (x1 * x1) + (-2.0 * x1 + 1.0)
    let test = { Name = ObjectiveName "Minimize x"; Type = Minimize; Expression = objective }
    let ahh = Objective.create "Minimize x" Minimize objective

    let constraint1 = Constraint.create "Name" (x1 ==> 0.0)
    let constraint2 = Constraint.create "Name" (x1 <== 0.5)
    
    let qp : QPProblem = {
        Objective = objective
        Constraints = seq {constraint1; constraint2}
    }
    let solver = AccordSolver.AccordSolver()
    let sol = solver.solve qp
    printfn "%A" sol