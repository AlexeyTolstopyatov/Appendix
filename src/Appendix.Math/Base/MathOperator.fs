namespace Appendix.Math.Base
//
// CoffeeLake (C) 2025-*
// This file contains abstractions under operators what applies
// to any objects in math. 
// 
// License: MIT
/// This is a numeric|computable operator type.
/// MathOperator can work only if target result and
/// mathematical object already represents as "value".
/// 
/// This means next: 
/// If you want to see result of computation what can be
/// a digit/vector or something what holds actually value
/// 
/// Don't apply it on symbolic computation. All types
/// what returns the "expression" are Symbolic-Typed operators! 
/// 
/// Abstraction under all functions what will be implemented in
/// other blocks in the DLL.
type MathOperator<'TObject, 'TResult> = {
    /// Processor
    Compute : 'TObject -> 'TResult
    /// Description/Declaration of function
    Declare : string
    /// Output of result.
    /// Applies like: 
    /// <c>let result_string = Operator.result("A") // <-- "Oper(A) = <!value>"</c>
    Symbol : string -> string
}