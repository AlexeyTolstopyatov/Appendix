module public Appendix.Math.Functions.Applications

open Appendix.Math.Base

type DifferentiationStep =
    | DfStart of expression: SymbolicExpression * variable: string
    | DfConst of expression: SymbolicExpression
    | DfVariable of variable: string * result: SymbolicExpression
    | DfSum of left: SymbolicExpression * right: SymbolicExpression * dl: SymbolicExpression * dr: SymbolicExpression
    | DfProduct of u: SymbolicExpression * v: SymbolicExpression * du: SymbolicExpression * dv: SymbolicExpression
    | DfQuotient of u: SymbolicExpression * v: SymbolicExpression * du: SymbolicExpression * dv: SymbolicExpression
    | DfPowerRule of baseExpr: SymbolicExpression * exponent: SymbolicExpression * result: SymbolicExpression
    | DfChain of outer: SymbolicExpression * inner: SymbolicExpression * dOuter: SymbolicExpression * dInner: SymbolicExpression
    | DfTrigonometric of functionName: string * argument: SymbolicExpression * result: SymbolicExpression
    | DfExponential of argument: SymbolicExpression * result: SymbolicExpression
    | DfLogarithm of argument: SymbolicExpression * result: SymbolicExpression
    | DfSimplify of from: SymbolicExpression * toExpr: SymbolicExpression
    | DfFinal of result: SymbolicExpression
type IntegrationStep =
    | Start of expression: SymbolicExpression * variable: string
    | FConstMultiple of expression: SymbolicExpression * SymbolicExpression * SymbolicExpression
    | FSum of left: SymbolicExpression * right: SymbolicExpression * dl: SymbolicExpression * dr: SymbolicExpression
    | FPower of baseExpr: SymbolicExpression * exponent: SymbolicExpression * result: SymbolicExpression
    | FTrigonometric of functionName: string * argument: SymbolicExpression * result: SymbolicExpression
    | FExponential of argument: SymbolicExpression * result: SymbolicExpression
    | FLogarithm of argument: SymbolicExpression * result: SymbolicExpression
    | FSubstitution of from: SymbolicExpression * toExpr: SymbolicExpression * substitution: string
    | FIntegrationByParts of u: SymbolicExpression * v: SymbolicExpression * result: SymbolicExpression
    | FPartialFractions of expression: SymbolicExpression * decomposition: SymbolicExpression
    | FSimplify of from: SymbolicExpression * toExpr: SymbolicExpression
    | FFinal of result: SymbolicExpression
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
[<CompiledName "DifferentiationOperator">]
let differentiationOperator : SymbolicMathOperator<SymbolicExpression * string, DifferentiationStep, SymbolicExpression> = {
    Steps = fun (expr, variable) ->
        let rec differentiate (e: SymbolicExpression) (steps: DifferentiationStep list) : SymbolicExpression * DifferentiationStep list =
            match e with
            | Number _ -> Number 0.0, steps @ [DfConst e]
            | Variable v when v = variable -> Number 1.0, steps @ [DfVariable (v, Number 1.0)]
            | Variable v -> Number 0.0, steps @ [DfVariable (v, Number 0.0)]
            | Add (u, v) ->
                let du, steps1 = differentiate u steps
                let dv, steps2 = differentiate v steps1
                let result = Add (du, dv)
                result, steps2 @ [DfSum (u, v, du, dv)]
            | Subtract (u, v) ->
                let du, steps1 = differentiate u steps
                let dv, steps2 = differentiate v steps1
                let result = Subtract (du, dv)
                result, steps2 @ [DfSum (u, v, du, dv)]
            | Multiply (u, v) ->
                let du, steps1 = differentiate u steps
                let dv, steps2 = differentiate v steps1
                let result = Add (Multiply (du, v), Multiply (u, dv))
                result, steps2 @ [DfProduct (u, v, du, dv)]
            | Divide (u, v) ->
                let du, steps1 = differentiate u steps
                let dv, steps2 = differentiate v steps1
                let numerator = Subtract (Multiply (du, v), Multiply (u, dv))
                let denominator = Power (v, Number 2.0)
                let result = Divide (numerator, denominator)
                result, steps2 @ [DfQuotient (u, v, du, dv)]
            | Power (u, Number n) ->
                let du, steps1 = differentiate u steps
                let result = Multiply (Multiply (Number n, Power (u, Number (n - 1.0))), du)
                result, steps1 @ [DfPowerRule (u, Number n, result)]
            | Sin u ->
                let du, steps1 = differentiate u steps
                let result = Multiply (Cos u, du)
                result, steps1 @ [DfTrigonometric ("sin", u, result)]
            | Cos u ->
                let du, steps1 = differentiate u steps
                let result = Multiply (Neg (Sin u), du)
                result, steps1 @ [DfTrigonometric ("cos", u, result)]
            | Exp u ->
                let du, steps1 = differentiate u steps
                let result = Multiply (Exp u, du)
                result, steps1 @ [DfExponential (u, result)]
            | Ln u ->
                let du, steps1 = differentiate u steps
                let result = Multiply (Divide (Number 1.0, u), du)
                result, steps1 @ [DfLogarithm (u, result)]
            | Neg u ->
                let du, steps1 = differentiate u steps
                let result = Neg du
                result, steps1 @ [DfChain (Neg u, u, result, du)]
            | _ -> 
                e, steps @ [DfFinal e] // Fallback for unsupported expressions
        
        let initialSteps = [DfStart (expr, variable)]
        let result, steps = differentiate expr initialSteps
        let simplifiedResult = Differentiation.simplify result
        let finalSteps = 
            if simplifiedResult <> result then
                steps @ [DfSimplify (result, simplifiedResult)]
            else
                steps
        finalSteps @ [DfFinal simplifiedResult]
    
    Result = fun steps ->
        let finalStep = steps |> List.last
        match finalStep with
        | DfFinal result -> result
        | _ -> 
            steps 
            |> List.tryPick (function 
                | DfConst e -> Some (Number 0.0)
                | DfVariable (_, result) -> Some result
                | DfSum (_, _, leftDeriv, rightDeriv) -> Some (Add (leftDeriv, rightDeriv))
                | DfProduct (u, v, du, dv) -> Some (Add (Multiply (du, v), Multiply (u, dv)))
                | DfFinal result -> Some result
                | _ -> None)
            |> Option.defaultValue (Variable "undefined")
    
    ToString = fun steps name ->
        let stepTexts = 
            steps 
            |> List.map (fun step ->
                match step with
                | DfStart (expr, var) -> $@"\frac{{d}}{{d{var}}}({AxStringify.texString expr})"
                | DfConst _ -> "0"
                | DfVariable (v, result) -> $@"\frac{{d}}{{d{v}}}({v}) = {AxStringify.texString result}"
                | DfSum (u, v, du, dv) -> $@"\frac{{d}}{{dx}}({AxStringify.texString u} + {AxStringify.texString v}) = {AxStringify.texString du} + {AxStringify.texString dv}"
                | DfProduct (u, v, du, dv) -> $@"\frac{{d}}{{dx}}({AxStringify.texString u} \cdot {AxStringify.texString v}) = {AxStringify.texString du} \cdot {AxStringify.texString v} + {AxStringify.texString u} \cdot {AxStringify.texString dv}"
                | DfQuotient (u, v, du, dv) -> $@"\frac{{d}}{{dx}}\left(\frac{{{AxStringify.texString u}}}{{{AxStringify.texString v}}}\right) = \frac{{{AxStringify.texString du} \cdot {AxStringify.texString v} - {AxStringify.texString u} \cdot {AxStringify.texString dv}}}{{({AxStringify.texString v})^2}}"
                | DfPowerRule (baseExpr, exponent, result) -> $@"\frac{{d}}{{dx}}({AxStringify.texString baseExpr})^{{{AxStringify.texString exponent}}} = {AxStringify.texString result}"
                | DfTrigonometric (fn, arg, result) -> $@"\frac{{d}}{{dx}}\{fn}({AxStringify.texString arg}) = {AxStringify.texString result}"
                | DfExponential (arg, result) -> $@"\frac{{d}}{{dx}}e^{{{AxStringify.texString arg}}} = {AxStringify.texString result}"
                | DfLogarithm (arg, result) -> $@"\frac{{d}}{{dx}}\ln({AxStringify.texString arg}) = {AxStringify.texString result}"
                | DfChain (outer, inner, outerDeriv, innerDeriv) -> $@"\frac{{d}}{{dx}}{AxStringify.texString outer} = {AxStringify.texString outerDeriv} \cdot {AxStringify.texString innerDeriv}"
                | DfSimplify (fromExpr, toExpr) -> $@"\text{{Simplification: }} {AxStringify.texString fromExpr} \Rightarrow {AxStringify.texString toExpr}"
                | DfFinal result -> $@"\boxed{{\frac{{d}}{{dx}} = {AxStringify.texString result}}}"
            )
        String.concat " \\\\ \n" stepTexts
    
    Declare = @"\frac{d}{dx} : \mathcal{C}^1 \to \mathcal{C}^0"
}
/// In mathematics, an integral is the continuous analog of a sum,
/// and is used to calculate areas, volumes, and their generalizations.
///
/// Integration, the process of computing an integral,
/// is one of the two fundamental operations of calculus, the other being differentiation.
/// Integration was initially used to solve problems in mathematics and physics,
/// such as finding the area under a curve, or determining displacement from velocity.
/// Usage of integration expanded to a wide variety of scientific fields thereafter.
[<CompiledName "IntegrationOperator">]
let integrationOperator : SymbolicMathOperator<SymbolicExpression * string, IntegrationStep, SymbolicExpression> = {
    Steps = fun (expr, variable) ->
        let rec integrate (e: SymbolicExpression) (steps: IntegrationStep list) : SymbolicExpression * IntegrationStep list =
            match e with
            | Number n -> Multiply (e, Variable variable), steps @ [FPower (Number n, Number 1.0, Multiply (Number n, Variable variable))]
            | Variable v when v = variable ->
                Divide (Power (Variable variable, Number 2.0), Number 2.0), 
                steps @ [FPower (Variable variable, Number 1.0, Divide (Power (Variable variable, Number 2.0), Number 2.0))]
            | Power (Variable v, Number n) when v = variable && n <> -1.0 ->
                Divide (Power (Variable variable, Number (n + 1.0)), Number (n + 1.0)),
                steps @ [FPower (Variable variable, Number n, Divide (Power (Variable variable, Number (n + 1.0)), Number (n + 1.0)))]
            | Power (Variable v, Number -1.0) when v = variable ->
                Ln (Variable variable), 
                steps @ [FLogarithm (Variable variable, Ln (Variable variable))]
            | Sin (Multiply (Number a, Variable v)) when v = variable ->
                Neg (Divide (Cos (Multiply (Number a, Variable variable)), Number a)),
                steps @ [FTrigonometric ("sin", Multiply (Number a, Variable variable), Neg (Divide (Cos (Multiply (Number a, Variable variable)), Number a)))]
            | Cos (Multiply (Number a, Variable v)) when v = variable ->
                Divide (Sin (Multiply (Number a, Variable variable)), Number a),
                steps @ [FTrigonometric ("cos", Multiply (Number a, Variable variable), Divide (Sin (Multiply (Number a, Variable variable)), Number a))]
            | Exp (Multiply (Number a, Variable v)) when v = variable ->
                Divide (Exp (Multiply (Number a, Variable variable)), Number a),
                steps @ [FExponential (Multiply (Number a, Variable variable), Divide (Exp (Multiply (Number a, Variable variable)), Number a))]
            | Add (u, v) ->
                let intU, steps1 = integrate u steps
                let intV, steps2 = integrate v steps1
                let result = Add (intU, intV)
                result, steps2 @ [FSum (u, v, intU, intV)]
            | Multiply (Number c, u) ->
                let intU, steps1 = integrate u steps
                let result = Multiply (Number c, intU)
                result, steps1 @ [FConstMultiple (Number c, u, result)]
            | _ ->
                let result = Add (Multiply (e, Variable variable), Variable "C")
                result, steps @ [FFinal result]
        let initialSteps = [Start (expr, variable)]
        let result, steps = integrate expr initialSteps
        let simplifiedResult = Differentiation.simplify result
        let finalSteps = 
            if simplifiedResult <> result then
                steps @ [FSimplify (result, simplifiedResult)]
            else
                steps
        // F(x) + const
        let finalResult = Add (simplifiedResult, Variable "C")
        finalSteps @ [FFinal finalResult]
        
    Result = fun steps ->
        let finalStep = steps |> List.last
        match finalStep with
        | FFinal result -> result
        | _ -> Add (Variable "\\text{undefined}", Variable "C")
    
    ToString = fun steps name ->
        let stepTexts = 
            steps 
            |> List.map (fun step ->
                match step with
                | Start (expr, var) -> $@"\int {AxStringify.texString expr} \, d{var}"
                | FPower (baseExpr, exponent, result) -> $@"\int {AxStringify.texString baseExpr}^{{{AxStringify.texString exponent}}} \, dx = {AxStringify.texString result}"
                | FTrigonometric (fn, arg, result) -> $@"\int \{fn}({AxStringify.texString arg}) \, dx = {AxStringify.texString result}"
                | FExponential (arg, result) -> $@"\int e^{{{AxStringify.texString arg}}} \, dx = {AxStringify.texString result}"
                | FLogarithm (arg, result) -> $@"\int \frac{{1}}{{{AxStringify.texString arg}}} \, dx = {AxStringify.texString result}"
                | FSubstitution (from, toExpr, subst) -> $@"\text{{Substitution: }} {subst} \Rightarrow {AxStringify.texString from} \to {AxStringify.texString toExpr}"
                | FIntegrationByParts (u, v, result) -> $@"\text{{by parts: }} \int {AxStringify.texString u} \, d{ AxStringify.texString v} = {AxStringify.texString result}"
                | FPartialFractions (expr, decomposition) -> $@"\text{{Partial fractions: }} {AxStringify.texString expr} = {AxStringify.texString decomposition}"
                | FSimplify (fromExpr, toExpr) -> $@"\text{{Simplification: }} {AxStringify.texString fromExpr} \Rightarrow {AxStringify.texString toExpr}"
                | FFinal result -> $@"\int {name} \, dx = {AxStringify.texString result}"

                | node -> failwith $"todo: %A{node}"
            )
        String.concat " \\\\ \n" stepTexts
    
    Declare = @"\int \cdot \, dx : \mathcal{C}^0 \to \mathcal{C}^1"
}