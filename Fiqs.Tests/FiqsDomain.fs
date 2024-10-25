module FiqsDomain

open Xunit
open Fiqs.Types
open Fiqs


[<Fact>]
let ``Test solver for a simple 1-n QP problem`` () =
    // f(x) = (x-1)^2 = x^2 -2x + 1 s.t. x <= 0.5 && x >= 0
    let x1 = Decision.createContinuous "x1" 0.0 100.0

    let objectiveExpression = x1 * x1 + (-2.0 * x1 + 1.0)
    
    let objective = Objective.create "Minimize x" Minimize objectiveExpression

    let constraint1 = Constraint.create "Name" (x1 ==> 0.0)
    let constraint2 = Constraint.create "Name" (x1 <== 0.5)
    
    let model = 
        Model.create objective
        |> Model.addConstraints [constraint1; constraint2]
    

    let solver = AccordSolver.AccordSolver()
    let sol = solver.solve model
    match sol with
    | Optimal solution ->
        Assert.Equal(solution.SolutionMap[DecisionName "x1"], 0.5, 1e-6)
    | _ ->
        Assert.Fail()

[<Fact>]
let ``Test solver for a simple 2-n QP problem`` () =
    // f(x) = x^2 + y^2 
    let x1 = Decision.createContinuous "x1" 0.0 100.0
    let x2 = Decision.createContinuous "x2" 0.0 100.0

    let objectiveExpression = x1 * x1 + x2 * x2 + x1 * x2 + (2.0 * x1)
    
    
    let objective = Objective.create "Minimize x" Minimize objectiveExpression

    let constraint1 = Constraint.create "Name" (x1 ==> 0.0)
    let constraint2 = Constraint.create "Name" (x2 ==> 0.0)
    
    let model = 
        Model.create objective
        |> Model.addConstraints [constraint1; constraint2]
    

    let solver = AccordSolver.AccordSolver()
    let sol = solver.solve model
    match sol with
    | Optimal solution ->
        Assert.Equal(solution.SolutionMap[DecisionName "x1"], 0.0, 1e-6)
        Assert.Equal(solution.SolutionMap[DecisionName "x2"], 0.0, 1e-6)
    | _ ->
        Assert.Fail()


    