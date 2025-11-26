namespace Appendix.Math.Functions
open Appendix.Math.Base

module AxIndefiniteIntegration =
    let rec define(x:string, expr: SymbolicExpression): SymbolicExpression =
        match expr with
        | Number _ -> Number 0.0
        