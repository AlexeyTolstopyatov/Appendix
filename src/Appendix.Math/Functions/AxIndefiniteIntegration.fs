namespace Appendix.Math.Functions

module AxIndefiniteIntegration =
    let rec define(x:string, expr: AxExpression): AxExpression =
        match expr with
        | Number _ -> Number 0.0
        