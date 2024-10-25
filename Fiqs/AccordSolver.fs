module AccordSolver
open System.Collections.Generic
open System.Linq
open Fiqs.Types
open Fiqs
open Accord.Math.Optimization
open MathNet.Numerics.LinearAlgebra

let convertToStandardForm (model: Model) =
    // Convert the objective function to Q and c
    let (cD, constantTerm, maybeQ) = 
        ObjectiveExpression.evaluate model.Objective.Expression
    
    // Handle cases where Q is missing
    let QD =
        match maybeQ with
        | Some qMatrix -> qMatrix
        | None -> raise (InputError "Augmented Lagrangian of the Accord-Solver has to have a twice differentiable function i.e. it has to have a quadratic part")

    // Unzip keys from the quadratic matrix and linear terms
    let (keys1, keys2) = QD.Keys |> List.ofSeq |> List.unzip
    let keys3 = cD.Keys |> List.ofSeq

    // Merge keys and ensure uniqueness
    let keys = HashSet(List.append keys1 (List.append keys2 keys3))
    
    let equalities = model.Constraints |> Seq.filter(fun x -> x.Expression.IsEquality) 
    let inequalities = model.Constraints |> Seq.filter(fun x -> x.Expression.IsInequality)

    let n = keys.Count
    let m = inequalities |> Seq.length
    let e = equalities |> Seq.length

    // Initialize matrices and vectors
    let Q = Matrix.Build.Dense(n, n, 0.0)   // Q: quadratic term matrix
    let A = Matrix.Build.Dense(m, n, 0.0)   // A: inequality constraint coefficient matrix
    let c = Vector.Build.Dense(n, 0.0)      // c: linear term vector
    let b = Vector.Build.Dense(m, 0.0)      // b: Inequality constraint value
    let E = Matrix.Build.Dense(e, n, 0.0)   // E: equality constraints
    let l = Vector.Build.Dense(e, 0.0)
    // Fill the c vector
    keys 
    |> Seq.iteri(fun i key ->
        if cD.ContainsKey(key) then
            c.[i] <- cD.[key]
        else
            c.[i] <- 0.0
    )

    // Fill the Q matrix
    Seq.allPairs keys keys
    |> Seq.iter(fun (key1, key2) ->
        let i = Seq.findIndex ((=) key1) keys  // Get index of key1
        let j = Seq.findIndex ((=) key2) keys  // Get index of key2
        if QD.ContainsKey((key1, key2)) then
            Q.[i, j] <- QD.[(key1, key2)] * 2.0
            Q.[j, i] <- QD.[(key1, key2)] * 2.0 // Ensure symmetry for Q
    )
   



    inequalities
    |> Seq.iteri (fun i constr ->
        let row, rhs = ConstraintExpression.evaluateConstraint constr.Expression
        keys 
        |> Seq.iteri(fun j key ->
            if row.ContainsKey(key) then
                A.[i, j] <- row.[key]
            else
                A.[i, j] <- 0.0
        )
        b.[i] <- rhs
    )

    equalities
    |> Seq.iteri (fun i constr ->
        let row, rhs = ConstraintExpression.evaluateConstraint constr.Expression
        keys 
        |> Seq.iteri(fun j key ->
            if row.ContainsKey(key) then
                E.[i, j] <- row.[key]
            else
                E.[i, j] <- 0.0
        )
        l.[i] <- rhs
    )

    // Return the matrices Q, c, A, b, E, l, constant term and all keys
    Q, c, A, b, E, l, constantTerm, keys
    

[<RequireQualifiedAccess>]
type AccordSolver() =
    member this.solve(model:Model) = (this :> ISolver).solve(model)

    interface ISolver with
        member this.solve (p: Model): SolveResult =
            let Q,c,A,b,E,l,constant,keys = convertToStandardForm p

            if Q <> Q.Transpose() then
                raise (InputError(sprintf "Q is not symmetric! Check inputs up to this point: %A" Q))

            let objective = QuadraticObjectiveFunction(Q.ToArray(),c.ToArray())

            let inequalities = 
                A.ToRowArrays()
                |> Array.mapi(
                    fun i row ->
                        let lincon = LinearConstraint(row)
                        lincon.ShouldBe <- ConstraintType.GreaterThanOrEqualTo
                        lincon.Value <- b.[i]
                        lincon
                )
                |> Seq.ofArray

            let equalities =
                E.ToRowArrays()
                |> Array.mapi(
                    fun i row ->
                        let lincon = LinearConstraint(row)
                        lincon.ShouldBe <- ConstraintType.EqualTo
                        lincon.Value <- l.[i]
                        lincon
                )
                |> Seq.ofArray

            let constraints : IEnumerable<IConstraint> = Seq.append inequalities equalities |> Seq.map (fun x -> x :> IConstraint)
            
            let alm = AugmentedLagrangian(objective, constraints)
            if alm.Minimize() then
                let sol = alm.Solution
                let solutionMap = Dictionary<DecisionName,float>()
                let keyArray = keys.ToArray()
                sol 
                |> Array.iteri (
                    fun i value ->
                        solutionMap.Add(keyArray[i], value)
                )

                Optimal { ObjectiveValue = alm.Value + constant; X = DenseVector.ofArray sol; SolutionMap = solutionMap }
            else
                Infeasible (ConvergenceException("Failed to converge"))
                
            

                