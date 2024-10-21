module Types
open System
open MathNet.Numerics.LinearAlgebra

// Input types

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
type Objective =
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