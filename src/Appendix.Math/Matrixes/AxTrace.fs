namespace Appendix.Math.Base

[<Class>]
type AxTraceOperator(d: double[,]) =
    inherit AxAbstractOperator<double[,], double>()
    let tr(matrix: double[,]) =
        let mutable result = 0.0
        for i = 0 to matrix.GetLength(0) do
            result <- result + matrix[i, i]
        
        result
    let _result: double = tr(d)
    /// Prints result
    override _.latex(name) =
        $@"\text{{tr}}{{{name}}}"
    /// In linear algebra, the trace of a square matrix A, 
    /// denoted tr(A) is the sum of the elements on its main diagonal
    override _.get = _result
    /// Declaration of current linear operator
    override _.abstractDecl =
        @"tr : L \longmapsto \mathbb{R}"