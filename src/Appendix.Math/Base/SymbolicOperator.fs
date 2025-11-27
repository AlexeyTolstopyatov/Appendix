namespace Appendix.Math.Base
// CoffeeLake (c) 2025-*
///
/// Abstract symbolic operator always returns the expression
/// or expression-string. This is what declares as expression
/// doesn't have a value but contains answer what can be 
/// reinterpreted like "object" which contains digit-value.
///    
type SymbolicMathOperator<'TObject, 'TStep, 'TResult> = {
    Steps: 'TObject -> 'TStep list
    Result: 'TStep list -> 'TResult
    ToString: 'TStep list -> string -> string
    Declare: string
}