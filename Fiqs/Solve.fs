namespace Fiqs
open Fiqs
open Fiqs.Types

[<RequireQualifiedAccess>]
module Solver = 
    let solve(model:Model) (settings:Settings) : SolveResult =
        match settings.SolverType with
        | AccordLagrangian ->
            let solver = AccordSolver.AccordSolver()
            solver.solve model


