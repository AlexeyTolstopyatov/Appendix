namespace Appendix.Math.Functions
open Appendix.Math.Base

module AxIndefiniteIntegration =
    let rec define(x:string, expr: AxExpression): AxExpression =
        match expr with
        | Number _ -> Number 0.0
        