namespace Fiqs.Solver


// Lösungsstrategie: 
// 1. Lineare Relaxation des Optimierungsproblems
// 2. Aktives-Set-Methode nutzen um iterativ ein relaxiertes Ergebnis zu erhalten
// 3. Mittels Branch-and-Cut annnährende Werte finden





module Solver =
    open MathNet.Numerics.LinearAlgebra

    