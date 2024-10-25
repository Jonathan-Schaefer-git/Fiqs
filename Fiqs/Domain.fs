namespace Fiqs
open System
open Fiqs.Types


[<RequireQualifiedAccess>]
type ISolver =
    abstract solve : Model -> SolveResult

[<RequireQualifiedAccess>]
module Decision =
    let create decisionName decisionType =
        {
            Name = DecisionName decisionName
            Type = decisionType
        }

    let createContinuous decisionName lower upper =
        {
            Name = DecisionName decisionName
            Type = Continuous(lower, upper)
        }

    let createInteger decisionName lower upper =
        {
            Name = DecisionName decisionName
            Type = Integer(lower, upper)
        }

[<RequireQualifiedAccess>]
module Constraint =
    let create constraintName (constraintBy:ConstraintExpression) =
        if String.IsNullOrEmpty(constraintName) then
            raise (InputError (sprintf "%s: Linear expression is empty" constraintName))
        {
            Constraint.Name = constraintName
            Constraint.Expression = constraintBy
        }

[<RequireQualifiedAccess>]
module ObjectiveExpression =
    let evaluate (expr: ObjectiveExpression) =
        match expr with
        | LinearOnly linearExpr ->
            // Evaluate just the linear part
            let A, b = LinearExpression.evaluate linearExpr
            A, b, None  // No quadratic part
        
        | QuadraticAndLinear (quadraticExpr, linearExpr) ->
            // Evaluate both linear and quadratic parts
            let Q = QuadraticExpression.evaluate quadraticExpr
            let c, constant = LinearExpression.evaluate linearExpr
            c, constant, Some Q  // Return both parts

[<RequireQualifiedAccess>]
module Objective =
    let create name objectiveSense expression =
        if String.IsNullOrEmpty(name) then
            raise (InputError "An objective requires an objective name")
        {
            Name = ObjectiveName name
            Type = objectiveSense
            Expression = expression
        }



[<RequireQualifiedAccess>]
module Model =
    let create (objective:Objective) =
        {
            Objective = objective
            Constraints = Seq.empty
        }

    let addConstraint (constr:Constraint) (model:Model) =
        { model with Constraints = (Seq.append model.Constraints (Seq.singleton constr))}

    let addConstraints (constraints:Constraint seq) (model:Model) =
        { model with Constraints = (Seq.append model.Constraints constraints)}


[<RequireQualifiedAccess>]
module Settings =
    let basic = {
        SolverType = AccordLagrangian
        TimeoutMs = 10_000 // 10s timeout
    }