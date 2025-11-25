namespace Appendix.Math

module AxStringify =
    open Appendix.Math.Functions

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
    let rec texString(expr: AxExpression) =
        match expr with
        | Number n -> 
            if n % 1.0 = 0.0 then string (int n)
            else $"%.2f{n}" // <-- Linear or Vertical simplified fractions needed.
        | Variable v -> v
        | Add (u, v) -> $"({texString u} + {texString v})"
        | Subtract (u, v) -> $"({texString u} - {texString v})"
        | Multiply (u, v) -> $"({texString u}*{texString v})"
        | Divide (u, v) -> $"({texString u}/{texString v})"
        | Power (u, v) -> $"({toString u})^{texString v}"
        | Sin e -> $"\\sin({texString e})"
        | Cos e -> $"\\cos({texString e})"
        | Exp e -> $"e^{{ {texString e} }}"
        | Ln e -> $"\\ln({texString e})"
        | Neg e -> $"-({texString e})"
        | Fraction(e1, e2) -> $"\\frac{texString e1}{texString e2}"