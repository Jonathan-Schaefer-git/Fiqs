namespace Fiqs.Types
open System
open System.Collections.Generic
open MathNet.Numerics.LinearAlgebra

// Input types
type DecisionType =
    | Boolean
    | Integer of LowerBoud:float * UpperBound:float
    | Continuous of LowerBound:float * UpperBound:float

type DecisionName = DecisionName of string


type Inequality =
    | Greater
    | Lesser

type Decision = {
    Name:DecisionName
    Type:DecisionType
    Multiplier:float
}

type Constraint = {
    Decisions: Decision list
    Value: float
    EqualityType: Inequality
}


type LinearExpression =
    | Empty
    | Scalar of float
    | Decision of Decision
    | AddFloat of float * LinearExpression
    | AddDecision of Decision * LinearExpression
    | Multiply of float * LinearExpression
    | AddLinearExpression of LinearExpression * LinearExpression

    static member evaluate (exp:LinearExpression) =
        let A = Dictionary<DecisionName, float>()  // Multiplier of decision variables
        let mutable b = 0.0  // Constant term or the "b" in Ax >= b

        let rec evaluateNode (exp: LinearExpression) : unit =
            match exp with
            | Scalar v -> b <- b + v  // Add constant term to b
            | Decision dec -> 
                // Add decision variable multiplier to A
                if A.ContainsKey(dec.Name) then
                    A.[dec.Name] <- A.[dec.Name] + dec.Multiplier
                else
                    A.Add(dec.Name, dec.Multiplier)
            | AddFloat (v, exp) ->
                b <- b + v  // Add the float to the constant term
                evaluateNode exp
            | AddDecision (dec, exp) ->
                if A.ContainsKey(dec.Name) then
                    A.[dec.Name] <- A.[dec.Name] + dec.Multiplier
                else
                    A.Add(dec.Name, dec.Multiplier)
                evaluateNode exp
            | Multiply (f, linExp) ->
                match linExp with
                | Decision dec ->
                    // Multiply the decision multiplier by f and add to A
                    let newMultiplier = f * dec.Multiplier
                    if A.ContainsKey(dec.Name) then
                        A.[dec.Name] <- A.[dec.Name] + newMultiplier
                    else
                        A.Add(dec.Name, newMultiplier)
                | Scalar sc -> 
                    b <- b + (f * sc)  // Scale the constant term
                | _ -> 
                    evaluateNode (Multiply(f, linExp))
            | AddLinearExpression (lhs, rhs) ->
                evaluateNode lhs
                evaluateNode rhs
            | Empty -> ()
        
        evaluateNode exp
        (A, b)  // Return the A matrix and constant b


// Q is the Hessian and represents the quadratic terms
// c is a Vector representing the linear terms
// lb and uB are lower and upper bounds on x respectively

type QPProblem = {
    Q: Matrix<float>
    c: Vector<float>
    lB: Vector<float>
    uB: Vector<float>

}




// Output types

exception SolverTimeOutException of string * TimeSpan
exception MaxIterationsException of string * int
exception SolverException of string

type Failure =
    | Timeout of SolverTimeOutException
    | Iterations of MaxIterationsException
    | Exception of SolverException


type Solution = {
    ObjectiveValue: float
    X: Vector<float>
    Iterations: UInt64
}

type Result =
    | Optimal of Solution
    | Infeasible of Failure

[<RequireQualifiedAccess>]
type ISolver =
    abstract solve : QPProblem -> Solution

type ObjectiveSense =
    | Maximize
    | Minimize

[<NoComparison>]
type Objective = {
    Name: string
    Type: ObjectiveSense
}

[<RequireQualifiedAccess>]
module Objective =
    let create objectiveName objectiveSense =
        if String.IsNullOrEmpty(objectiveName) then
            failwith "An objective requires an objective name"
        {
            Name = objectiveName
            Type = objectiveSense
        }
    

type Model =
    member create 


type Solver =
    | FiqsBarrier


type Settings = {
    Solver: Solver
    TimeoutMs: int
}