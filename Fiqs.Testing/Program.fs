open MathNet.Numerics.LinearAlgebra
open System.Collections.Generic
open System



type DecisionType =
    | Boolean
    | Integer of LowerBound:float * UpperBound:float
    | Continuous of LowerBound:float * UpperBound:float

type DecisionName = DecisionName of string

type Inequality =
    | Greater
    | Lesser

type Decision = {
    Name: DecisionName
    Type: DecisionType
}
with
    static member (*) (lhs: float, rhs: Decision) =
        Multiply(lhs, Decision rhs)

    static member (*) (lhs: Decision, rhs: Decision) =
        MultiplyDecision(lhs, rhs)

and Constraint = {
    Name:string
    Constraint:ConstraintExpression
}

and Expression =
    | Linear of LinearExpression
    | Quadratic of QuadraticExpression

with
    static member (+) (lhs:QuadraticExpression,rhs:LinearExpression) =
        QuadraticAndLinear(lhs,rhs)

and ConstraintExpression =
    | Inequality of lhs:Expression * Inequality * rhs:Expression
    | Equality of lhs:Expression * rhs:Expression

    static member evaluateConstraint (constrained: ConstraintExpression) : Dictionary<DecisionName, float> * float =
        let subtractDictionaries (A1: Dictionary<DecisionName, float>) (A2: Dictionary<DecisionName, float>) =
            // Subtract A2 from A1
            for kvp in A2 do
                if A1.ContainsKey(kvp.Key) then
                    A1.[kvp.Key] <- A1.[kvp.Key] - kvp.Value
                else
                    A1.Add(kvp.Key, -kvp.Value)
            A1
        
        match constrained with
        | Equality (lhs, rhs) ->
            match lhs, rhs with
            | Linear l, Linear r ->
                let A1, b1 = LinearExpression.evaluate l
                let A2, b2 = LinearExpression.evaluate r
                let A = subtractDictionaries A1 A2
                let b = b1 - b2
                (A, b)
            | _ -> raise (NotImplementedException("No support for quadratic constraints yet"))

        | Inequality (lhs, ineq, rhs) ->
            match lhs, rhs with
            | Linear l, Linear r ->
                let A1, b1 = LinearExpression.evaluate l
                let A2, b2 = LinearExpression.evaluate r
                let A = subtractDictionaries A1 A2
                let b = b1 - b2
                match ineq with
                | Lesser ->
                    // Flip sign for "<=" to convert to "A * x >= b"
                    let A' = A |> Seq.map (fun kvp -> kvp.Key, -kvp.Value) |> dict |> Dictionary
                    let b' = b
                    (A', b')
                | Greater ->
                    (A, b)
            | _ -> raise (NotImplementedException("No support for quadratic constraints yet"))

    static member (+) (rhs:QuadraticExpression, lhs:LinearExpression) =
        QuadraticAndLinear(rhs, lhs)


and QuadraticExpression =
    | MultiplyDecision of Decision * Decision
    | MultiplyDecisionScalar of float * Decision * Decision
    | SquareDecision of Decision
    | AddQuadraticExpression of QuadraticExpression * QuadraticExpression
    | ScaleQuadraticExpression of float * QuadraticExpression

    // Recursive evaluation of QuadraticExpression
    static member evaluate (exp: QuadraticExpression) =
        let Q = Dictionary<Tuple<DecisionName, DecisionName>, float>()

        let rec evaluateQuadratic qExp =
            match qExp with
            | MultiplyDecision (d1, d2) ->
                if Q.ContainsKey((d1.Name, d2.Name)) then
                    Q.[(d1.Name, d2.Name)] <- Q.[(d1.Name, d2.Name)] + 1.0
                else
                    Q.Add((d1.Name, d2.Name), 1.0)
            | MultiplyDecisionScalar (f, d1, d2) ->
                if Q.ContainsKey((d1.Name, d2.Name)) then
                    Q.[(d1.Name, d2.Name)] <- Q.[(d1.Name, d2.Name)] + f
                else
                    Q.Add((d1.Name, d2.Name), f)
            | SquareDecision d ->
                if Q.ContainsKey((d.Name, d.Name)) then
                    Q.[(d.Name, d.Name)] <- Q.[(d.Name, d.Name)] + 1.0
                else
                    Q.Add((d.Name, d.Name), 1.0)
            | AddQuadraticExpression (q1, q2) ->
                evaluateQuadratic q1
                evaluateQuadratic q2
            | ScaleQuadraticExpression (scalar, inner) ->
                let scaledQ = QuadraticExpression.evaluate inner
                for KeyValue ((d1, d2), value) in scaledQ do
                    if Q.ContainsKey((d1, d2)) then
                        Q.[(d1, d2)] <- Q.[(d1, d2)] + scalar * value
                    else
                        Q.Add((d1, d2), scalar * value)
        evaluateQuadratic exp
        Q  // Return the quadratic coefficient matrix

    // Addition of two QuadraticExpressions
    static member (+) (lhs: QuadraticExpression, rhs: QuadraticExpression) =
        AddQuadraticExpression(lhs, rhs)

    // Subtraction of two QuadraticExpressions (lhs - rhs)
    static member (-) (lhs: QuadraticExpression, rhs: QuadraticExpression) =
        AddQuadraticExpression(lhs, ScaleQuadraticExpression(-1.0, rhs))



and LinearExpression =
    | Empty
    | Scalar of float
    | Decision of Decision
    | AddFloat of float * LinearExpression
    | AddDecision of Decision * LinearExpression
    | Multiply of float * LinearExpression
    | AddLinearExpression of LinearExpression * LinearExpression

    static member evaluate (exp: LinearExpression) =
        let A = Dictionary<DecisionName, float>()  // Multiplier of decision variables
        let mutable b = 0.0  // Constant term or the "b" in Ax >= b
    
        // Helper function to multiply through the subtree
        let rec applyFactor factor exp =
            match exp with
            | Scalar c -> b <- b + (factor * c)  // Multiply constants
            | Decision d ->
                if A.ContainsKey(d.Name) then
                    A.[d.Name] <- A.[d.Name] + factor  // Accumulate multiplied coefficients
                else
                    A.Add(d.Name, factor)
            | AddFloat (f, inner) ->
                b <- b + (factor * f)  // Apply factor to float and continue evaluation
                applyFactor factor inner
            | AddDecision (d, inner) ->
                applyFactor factor (Decision d)  // Evaluate decision with factor
                applyFactor factor inner
            | Multiply (f, inner) ->
                applyFactor (factor * f) inner  // Multiply the factor and propagate
            | AddLinearExpression (lhs, rhs) ->
                applyFactor factor lhs  // Apply factor to both sides of the expression
                applyFactor factor rhs
            | Empty -> ()
    
        // Initial call to evaluate expression with a factor of 1.0
        applyFactor 1.0 exp
        A, b

    // Overriding the + operator
    static member (+) (lhs: LinearExpression, rhs: LinearExpression) =
        match lhs, rhs with
        | Scalar a, Scalar b -> Scalar (a + b)
        | Scalar a, _ -> AddFloat (a, rhs)
        | _, Scalar b -> AddFloat (b, lhs)
        | _ -> AddLinearExpression(lhs, rhs)

    static member (+) (lhs:LinearExpression, rhs:float) =
        match lhs with
        | Scalar a -> Scalar (a + rhs) 
        | _ -> AddFloat(rhs, lhs)


    // Overriding the - operator
    static member (-) (lhs: LinearExpression, rhs: LinearExpression) =
        match lhs, rhs with
        | Scalar a, Scalar b -> Scalar (a - b)
        | Scalar a, _ -> AddFloat (a, Multiply(-1.0, rhs))
        | _, Scalar b -> AddFloat (-b, lhs)
        | _ -> AddLinearExpression(lhs, Multiply(-1.0, rhs))

    // Overriding the * operator to handle Decision variables multiplication
    static member (*) (lhs: Decision, rhs: Decision) =
        MultiplyDecision(lhs, rhs)

    static member (+) (lhs:QuadraticExpression,rhs:LinearExpression) =
        QuadraticAndLinear(lhs,rhs)
    
    static member (*) (lhs: float, rhs: Decision) =
        Multiply(lhs, Decision rhs)
    
    static member (*) (lhs: Decision, rhs: float) =
        rhs * lhs
    
    static member (*) (lhs: LinearExpression, rhs: LinearExpression) =
        match lhs, rhs with
        | Scalar a, Scalar b -> Scalar (a * b)
        | Scalar a, _ -> Multiply(a, rhs)
        | _, Scalar b -> Multiply(b, lhs)
        | _ -> failwith "Non-linear terms not supported for * between non-scalars."

    static member (<==) (lhs:LinearExpression, rhs:float): ConstraintExpression =
        Inequality (Linear lhs, Lesser, Linear (Scalar rhs))

    static member (<==) (lhs:LinearExpression, rhs:LinearExpression): ConstraintExpression =
        Inequality (Linear lhs, Lesser, Linear rhs)

    static member (===) (lhs:LinearExpression, rhs:float) : ConstraintExpression =
        Equality(Linear lhs, Linear(Scalar rhs))

    static member(===) (lhs:LinearExpression, rhs:LinearExpression) : ConstraintExpression =
        Equality(Linear lhs, Linear rhs)

    static member (==>) (lhs:LinearExpression, rhs:float) : ConstraintExpression =
        Inequality(Linear lhs, Greater, Linear(Scalar rhs))

and ObjectiveExpression =
    | LinearOnly of LinearExpression
    | QuadraticAndLinear of QuadraticExpression * LinearExpression

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


// Example usage
let x1 = { Name = DecisionName("x1"); Type = Continuous(0.0, 100.0) }
let x2 = { Name = DecisionName("x2"); Type = Continuous(0.0, 100.0) }

let objective = (x1 * x1) - (x2 * x1) + (x2 * x2) + (3.0 * x1 + 5.0)
printfn "%A" (ObjectiveExpression.evaluate (objective))

//let linconst = 3.0 * x1 <== 2.0 * x2 + 9.0 + 4.0
//let A, b = ConstraintExpression.evaluateConstraint linconst
//printfn "A: %A" A
//printfn "b: %A" b

type Solver =
    | Accord
    | Fiqs

type SolverSettings = {
    Solver:Solver
    Timeout:int
}
