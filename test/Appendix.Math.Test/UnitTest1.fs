module Appendix.Math.Test

open NUnit.Framework
open Appendix.Math.Functions
open Appendix.Math.Base
open Appendix.Math.Matrixes

[<SetUp>]
let Setup () =
    ()

///
/// Get the derivative function from
/// f(x) = P_n(x), n \in N
///  
[<Test>]
let checkDerivativePolinomialFunctions () =
    let x = Variable "x"
    let f = (Power (x, Number 3.0), Multiply(x, Variable "a"))
                    |> Add
                    |> AxDifferentiation.define "x"
                    |> AxDifferentiation.simplify
                    |> AxStringify.toString
    Assert.Pass(f) 
///
/// Get the derivative function df(x)/dx from
/// parts of sum which are different nature
/// f(x) = P_n(x) + th(x)
/// 
[<Test>]
let checkDerivativeOfDifferentAlgebraicFunctions () =
    let x = Variable "x"
    let mother = (Power (x, Number 3.0), Sin x)
                    |> Add
                    |> AxStringify.toString
    let daughter = (Power (x, Number 3.0), Sin x)
                    |> Add
                    |> AxDifferentiation.define "x" // explicit set "dx"
                    |> AxDifferentiation.simplify
                    |> AxStringify.toString
                 // |> Console.WriteLine
    
    Assert.Pass $"{mother}\r\n{daughter}"

[<Test>]
let checkPermanentOf2x2E () = 
    let mat = array2D [ [ 1.0; 0.0]; [0.0; 1.0] ]
    let perm = mat |> Applications.permanentOperator.Compute

    Assert.Pass $"perm(E) = {perm}"
[<Test>]
let checkDeterminantOf2x2E () = 
    let mat = array2D [ [ 1.0; 0.0]; [0.0; 1.0] ]
    let det = Applications.determinantOperator.Compute mat

    Assert.Pass $"det(E) = {det}"

[<Test>]
let checkApplicationsOf2x2Mat () = 
    let mat = array2D [ [2.0; 8.0]; [3.0; 0.0] ]
    // det(A) = -24 expected
    // per(A) = +24 expected
    let perm = Applications.permanentOperator.Compute mat
    let det = Applications.determinantOperator.Compute mat

    Assert.Pass $"perm(A) = {perm}\r\ndet(A) = {det}"