namespace Appendix.Math.Base
/// This is a type of object which can work correct
/// with SymbolicOperator{T, S, R}s in functions module
/// 
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

module AxStringify =
    [<CompiledName "ToString">]
    let rec toString expr =
        match expr with
        | Number n -> 
            if n % 1.0 = 0.0 then string (int n)
            else $"%.2f{n}"
        | Variable v -> v
        | Add (u, v) -> $"({toString u} + {toString v})"
        | Subtract (u, v) -> $"({toString u} - {toString v})"
        | Multiply (u, v) -> $"({toString u}*{toString v})"
        | Divide (u, v) -> $"({toString u}/{toString v})"
        | Power (u, v) -> $"({toString u})^{toString v}"
        | Sin e -> $"sin({toString e})"
        | Cos e -> $"cos({toString e})"
        | Exp e -> $"exp({toString e})"
        | Ln e -> $"ln({toString e})"
        | Neg e -> $"-({toString e})"
        | Fraction (e1, e2) -> $"({toString e1}/{toString e2})"

    [<CompiledName "TexString">]
    let rec texString(expr: SymbolicExpression) =
        match expr with
        | Number n -> 
            if n % 1.0 = 0.0 then string (int n)
            else $"%.2f{n}" // <-- Linear or Vertical simplified fractions needed.
        | Variable v -> v
        | Add (u, v) -> $"({texString u} + {texString v})"
        | Subtract (u, v) -> $"({texString u} - {texString v})"
        | Multiply (u, v) -> $"{texString u} \\cdot {texString v}"
        | Divide (u, v) -> $"\\frac{texString u}{texString v}"
        | Power (u, v) -> $"({texString u})^{texString v}"
        | Sin e -> $"\\sin({texString e})"
        | Cos e -> $"\\cos({texString e})"
        | Exp e -> $"e^{{ {texString e} }}"
        | Ln e -> $"\\ln({texString e})"
        | Neg e -> $"-({texString e})"
        | Fraction(e1, e2) -> $"\\frac{texString e1}{texString e2}"