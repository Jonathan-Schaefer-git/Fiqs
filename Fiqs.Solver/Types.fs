module Types
open MathNet.Numerics
open MathNet.Numerics.LinearAlgebra


type QPProblem = {
    Q: Matrix<float>
    c: Vector<float>
    A: Matrix<float>
    y: Vector<float>
    b: Vector<float>
    lowerBounds: Vector<float>
    upperBounds: Vector<float>
    tolerance:float
}