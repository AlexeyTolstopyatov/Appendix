module Appendix.Math.Test

open NUnit.Framework
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
    let expr = (Power(Variable "x", Number 3), Ln(Variable "x"))
                |> Add
    let steps = Functions.Applications.differentiationOperator.Steps (expr, "x")
                |> Functions.Applications.differentiationOperator.Result
                |> AxStringify.texString
    
    Assert.Pass($"{steps}") 
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
    
    let daughter =
        Functions.Applications.differentiationOperator.Steps (Add((Power (x, Number 3.0), Sin x)), "x")
        |> Functions.Applications.differentiationOperator.Result
        |> AxStringify.toString
    
    Assert.Pass $"{mother}\r\n{daughter}"

[<Test>]
let checkIndefiniteIntegration () =
    let x = Variable "x"
    let daughter =
        Functions.Applications.integrationOperator.Steps (Add((Power (x, Number 3.0), Sin x)), "x")
        |> Functions.Applications.integrationOperator.Result
        |> AxStringify.texString
        
    Assert.Pass daughter

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

[<Test>]
let checkNullityOf () =
    let matrix = array2D [[1.0; 2.0; 1.0]; [2.0; 4.0; 2.0]]

    let steps = Applications.kernelOperator.Steps matrix
    let solution = Applications.kernelOperator.Result steps
    let latex = Applications.kernelOperator.ToString steps "A"
    
    printfn $"%A{solution}"
    Assert.Pass latex

[<Test>]
let checkImageOf () =
    let matrix = array2D [[1.0; 2.0; 1.0]; [2.0; 4.0; 2.0]]

    let steps = Applications.imageOperator.Steps matrix
    let solution = Applications.imageOperator.Result steps
    let latex = Applications.imageOperator.ToString steps "A"
    
    printfn $"%A{solution}"
    Assert.Pass latex

[<Test>]
let checkUndefinedNullity () = 
    let matrix = array2D [
        [1.0; 2.0; 1.0]; 
        [0.0; 0.0; 2.0]
    ]

    let steps = Applications.kernelOperator.Steps matrix
    let solution = Applications.kernelOperator.Result steps
    let latex = Applications.kernelOperator.ToString steps "A"

    printfn $"%A{solution}"
    Assert.Pass latex