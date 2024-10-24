namespace Fiqs.Types
open System



[<RequireQualifiedAccess>]
type ISolver =
    abstract solve : QPProblem -> Solution


[<RequireQualifiedAccess>]
module Constraint =
    let create constraintName (linear:LinearExpression) =
        if linear.IsEmpty then
            raise (InputError (sprintf "%s: Linear expression is empty" constraintName))


//module Objective =
//    let createObj name objectiveSense expression =
//        if String.IsNullOrEmpty(name) then
//            raise (InputError "An objective requires an objective name")
//        {
//            Name = ObjectiveName name
//            Type = objectiveSense
//            Expression = expression
        //}

type Model = {
    Objective:ObjectiveExpression
    Constraints:Constraint seq
}


[<RequireQualifiedAccess>]
module Model =
    let create (objective:ObjectiveExpression) =
        {
            Objective = objective
            Constraints = Seq.empty
        }

    let addConstraint (constr:Constraint) (model:Model) =
        { model with Constraints = (Seq.append model.Constraints (Seq.singleton constr))}

    let addConstraints (constraints:Constraint seq) (model:Model) =
        { model with Constraints = (Seq.append model.Constraints constraints)}