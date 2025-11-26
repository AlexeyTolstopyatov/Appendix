namespace Appendix.Math.Base
// CoffeeLake (C) 2025-*
// This file contains abstractions under "symbolic" operators 
// and symbolic computation in math. 
[<AbstractClass>]
type AxAbstractSymbolicOperator() =
    abstract member define : string * AxExpression -> AxExpression
    abstract member simplify : AxExpression -> AxExpression
///
/// Abstract symbolic operator always returns the expression
/// or expression-string. This is what declares as expression
/// doesn't have a value but contains answer what can be 
/// reinternpret like "object" which contains digit-value.
///    
type SymbolicMathOperator = {
    Translate: string -> AxExpression
    Define:    string * AxExpression -> AxExpression
    Compute:   AxExpression -> AxExpression
    Result:    AxExpression -> string
}