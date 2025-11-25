module Appendix.Math.Test

open NUnit.Framework
open Appendix.Math.Functions

[<SetUp>]
let Setup () =
    ()

///
/// Get the derivative function from
/// f(x) = P_n(x), n \in N
///  
[<Test>]
let checkSumOfFunctionsSameNature () =
    let x = Variable "x"
    let f = (Power (x, Number 3.0), Multiply(x, Variable "a"))
                    |> Add
                    |> AxDifferentiation.define "x"
                    |> AxDifferentiation.simplify
                    |> AxStringify.toString
    Assert.Pass 
///
/// Get the derivative function df(x)/dx from
/// parts of sum which are different nature
/// f(x) = P_n(x) + th(x)
/// 
[<Test>]
let checkSumOfFunctionsDifferentNature () =
    let x = Variable "x"
    let f = (Power (x, Number 3.0), Sin x)
                    |> Add
                    |> AxDifferentiation.define "x" // explicit set "dx"
                    |> AxDifferentiation.simplify
                    |> AxStringify.toString
                 // |> Console.WriteLine
    
    Assert.Pass()