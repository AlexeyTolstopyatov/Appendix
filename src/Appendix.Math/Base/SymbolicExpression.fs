namespace Appendix.Math.Base

type public SymbolicExpression =
    | Number of float
    | Variable of string
    | Fraction of SymbolicExpression * SymbolicExpression
    | Add of SymbolicExpression * SymbolicExpression
    | Subtract of SymbolicExpression * SymbolicExpression
    | Multiply of SymbolicExpression * SymbolicExpression
    | Divide of SymbolicExpression * SymbolicExpression
    | Power of SymbolicExpression * SymbolicExpression
    | Sin of SymbolicExpression
    | Cos of SymbolicExpression
    | Exp of SymbolicExpression
    | Ln of SymbolicExpression
    | Neg of SymbolicExpression