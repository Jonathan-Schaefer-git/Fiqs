module AccordSolver

open Fiqs.Types
open Accord.Math.Optimization



type AccordSolver =
    interface ISolver with
        member this.solve (p: QPProblem): Solution =
            let objFun = QuadraticObjectiveFunction(p.Q.ToArray(), p.c.ToArray())
            let alm = AugmentedLagrangian(objFun,)