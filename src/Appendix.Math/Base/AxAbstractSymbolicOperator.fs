namespace Appendix.Math.Base

[<AbstractClass>]
type AxAbstractSymbolicOperator() =
    abstract member define : string * AxExpression -> AxExpression
    abstract member simplify : AxExpression -> AxExpression
    