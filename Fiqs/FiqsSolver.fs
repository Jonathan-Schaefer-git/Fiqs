module FiqsSolver

open Types
open MathNet.Numerics.LinearAlgebra
open Fiqs.Solver


type FiqsSolver =
    interface ISolver with
        member this.solve (p: QPProblem): Solution =
            raise (System.NotImplementedException())