#r "nuget:MathNet.Numerics.FSharp"
#r "nuget:Flips"
open MathNet.Numerics.LinearAlgebra
open System
open Flips


let test1 = Decision.createContinuous "x1" 0.0 1.0
let test2 = Decision.createContinuous "x2" 0.0 1.0

let exp = test1 + test2