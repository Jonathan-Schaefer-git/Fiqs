open MathNet.Numerics.LinearAlgebra
open System.Collections.Generic


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
}

type Constraint = {
    Multipliers: Dictionary<DecisionName,float>
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
            | Scalar c -> b <- b + c                // Constant terms contribute to b
            | Decision d -> 
                if A.ContainsKey(d.Name) then
                    A.[d.Name] <- A.[d.Name] + 1.0  // Accumulate coefficients
                else
                    A.Add(d.Name, 1.0)
            | AddFloat (f, inner) -> 
                b <- b + f                          // Add the float to the constant part
                evaluateNode inner
            | AddDecision (d, inner) ->
                evaluateNode (Decision d)           // Evaluate the decision part
                evaluateNode inner
            | Multiply (f, inner) ->
                match inner with
                | Decision d -> 
                    if A.ContainsKey(d.Name) then
                        A.[d.Name] <- A.[d.Name] + f  // Apply scalar multiplication to decision
                    else
                        A.Add(d.Name, f)
                | Scalar c -> b <- b + (f * c)      // Apply scalar multiplication to constant
                | _ -> evaluateNode inner
            | AddLinearExpression (lhs, rhs) ->
                evaluateNode lhs                    // Evaluate both sides
                evaluateNode rhs
            | Empty -> ()
        
        evaluateNode exp
        { Multipliers = A; Value = b; EqualityType = Lesser}

let convertToStandardForm (constrained: Constraint) : Dictionary<DecisionName, float> * float =
    let A = constrained.Multipliers
    let b = constrained.Value
    match constrained.EqualityType with
    | Lesser ->
        // For "<=", we need to flip the sign to conform to "A * x >= b"
        let A' = A |> Seq.map (fun kvp -> kvp.Key, -kvp.Value) |> dict |> Dictionary
        let b' = -b + constrained.Value
        (A', b')
    | Greater ->
        // For ">=", we already have the form "A * x >= b"
        let b' = b - constrained.Value
        (A, b')

let x = { Name = DecisionName("x1"); Type = Continuous(0.0, 100.0) }

let expression = AddFloat(4.0,Multiply(2.0,Decision(x)))

let constraint = LinearExpression.evaluate expression 
let A,b = constraint |> convertToStandardForm
printfn "%A" constraint
printfn "%A %A" A b