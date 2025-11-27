namespace Appendix.Math.Functions
open Appendix.Math.Base
/// In mathematics, the derivative is a fundamental tool 
/// that quantifies the sensitivity to change of a function's output 
/// with respect to its input. The derivative of a function of a 
/// single variable at a chosen input value, when it exists, 
/// is the slope of the tangent line to the graph of the function at that point. 
/// 
/// The tangent line is the best linear approximation of 
/// the function near that input value. 
/// The derivative is often described as the instantaneous rate of change, 
/// the ratio of the instantaneous change in the dependent variable 
/// to that of the independent variable. 
/// 
/// The process of finding a derivative is called differentiation.
module private Differentiation =
    [<CompiledName("Define")>]
    let rec define x expr =
        match expr with
        | Number _ -> Number 0.0
        | Variable v when v = x -> Number 1.0 // d(x)dx
        | Variable _ -> Number 0.0
        | Add (u, v) -> Add (define x u, define x v)
        | Subtract (u, v) -> Subtract (define x u, define x v)
        | Multiply (u, v) -> 
            Add (Multiply (define x u, v), Multiply (u, define x v))
        | Divide (u, v) -> 
            Divide (
                Subtract (Multiply (define x u, v), Multiply (u, define x v)),
                Power (v, Number 2.0)
            )
        | Power (u, Number n) -> 
            Multiply (Multiply (Number n, Power (u, Number (n-1.0))), define x u)
        | Sin u -> Multiply (Cos u, define x u)
        | Cos u -> Multiply (Neg (Sin u), define x u)
        | Exp u -> Multiply (Exp u, define x u)
        | Ln u -> Multiply (Divide (Number 1.0, u), define x u)
        | Neg u -> Neg (define x u)
        | _ -> failwithf $"Unsupported expression: %A{expr}"    
    
    let rec simplify expr =
        match expr with
        | Add (Number a, Number b) -> Number (a + b)
        | Add (Number 0.0, e) | Add (e, Number 0.0) -> simplify e
        | Subtract (e, Number 0.0) -> simplify e
        | Subtract (Number 0.0, e) -> Neg (simplify e)
        | Multiply (Number 0.0, _) | Multiply (_, Number 0.0) -> Number 0.0
        | Multiply (Number 1.0, e) | Multiply (e, Number 1.0) -> simplify e
        | Divide (e, Number 1.0) -> simplify e
        | Divide (Number 0.0, _) -> Number 0.0
        | Power (e, Number 1.0) -> simplify e
        | Power (_, Number 0.0) -> Number 1.0
        | Neg (Neg e) -> simplify e
        | Neg (Number n) -> Number (-n)
        | Add (u, v) -> 
            match (simplify u, simplify v) with
            | Number a, Number b -> Number (a + b)
            | su, sv -> Add (su, sv)
        | Multiply (u, v) -> 
            match (simplify u, simplify v) with
            | Number a, Number b -> Number (a * b)
            | su, sv -> Multiply (su, sv)
        | Power (u, v) -> 
            match (simplify u, simplify v) with
            | Number a, Number b -> Number (a ** b)
            | su, sv -> Power (su, sv)
        | Sin e -> Sin (simplify e)
        | Cos e -> Cos (simplify e)
        | Exp e -> Exp (simplify e)
        | Ln e -> Ln (simplify e)
        | Neg e -> Neg (simplify e)
        | Divide (u, v) -> Divide (simplify u, simplify v)
        | Subtract (u, v) -> Subtract (simplify u, simplify v)
        | e -> e