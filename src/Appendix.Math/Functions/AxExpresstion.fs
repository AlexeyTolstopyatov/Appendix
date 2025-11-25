namespace Appendix.Math

type public AxExpression =
    | Number of float
    | Variable of string
    | Fraction of AxExpression * AxExpression
    | Add of AxExpression * AxExpression
    | Subtract of AxExpression * AxExpression
    | Multiply of AxExpression * AxExpression
    | Divide of AxExpression * AxExpression
    | Power of AxExpression * AxExpression
    | Sin of AxExpression
    | Cos of AxExpression
    | Exp of AxExpression
    | Ln of AxExpression
    | Neg of AxExpression
type public AxIntegersMatrix = int32[,]
type public AxFloatingMatrix = float[,]