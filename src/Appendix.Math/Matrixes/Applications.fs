module Appendix.Math.Matrixes.Applications
open Appendix.Math.Base
open System.Threading.Tasks
// CoffeeLake 2025-*
// This file contains all applications to matrixes
//      - determinant
//      - permanent
//      - tracing
//      - nullity (kernel set) definition
// All this operator applies to one-object.
// In other words I define it like Functions because
// operator means by itselves something between two or more operands
// but default once-argument function applies to one operand/object 
/// In mathematics, the determinant is a scalar-valued function of the entries of a square matrix. 
/// The determinant of a matrix A is commonly denoted det(A), det A, or |A|. 
/// Its value characterizes some properties of the matrix and 
/// the linear map represented, on a given basis, by the matrix. 
/// 
/// In particular, the determinant is nonzero 
/// if and only if the matrix is invertible and the corresponding 
/// linear map is an isomorphism. 
/// 
/// However, if the determinant is zero, 
/// the matrix is referred to as singular, meaning it does not have an inverse.
[<CompiledName "DeterminantOperator">]
let determinantOperator : MathOperator<double[,], double> = {
    Compute = fun mat -> 
        let rec det (mat: double[,]) : double =
            let n = mat.GetLength 0
            if n = 1 then mat[0,0]
            elif n = 2 then mat[0,0] * mat[1,1] - mat[0,1] * mat[1,0]
            else
                /// Takes slice of target minor.
                let getMinor (m: double[,]) row col =
                    let size = m.GetLength 0
                    Array2D.init (size - 1) (size - 1) (fun i j ->
                        let rowIndex = if i < row then i else i + 1
                        let colIndex = if j < col then j else j + 1
                        m[rowIndex, colIndex])

                [0 .. n - 1]
                |> List.map (fun col ->
                    let minor = getMinor mat 0 col
                    async {
                        let sign = if col % 2 = 0 then 1.0 else -1.0
                        let detMinor = det minor
                        return sign * mat[0, col] * detMinor
                    })
                |> Async.Parallel
                |> Async.RunSynchronously
                |> Array.sum
        let result = task {
            let n = mat.GetLength 0
            if n <= 2 then
                return det mat
            else
                let getMinor (m: double[,]) row col =
                    let size = m.GetLength 0
                    Array2D.init (size - 1) (size - 1) (fun i j ->
                        let rowIndex = if i < row then i else i + 1
                        let colIndex = if j < col then j else j + 1
                        m[rowIndex, colIndex])

                let tasks =
                    [0 .. n - 1]
                    |> List.map (fun col ->
                        let minor = getMinor mat 0 col
                        Task.Run(fun () ->
                            let sign = if col % 2 = 0 then 1.0 else -1.0
                            sign * mat[0, col] * det minor))

                let! results = Task.WhenAll(tasks)
                return Array.sum results 
        }
        result.Result // :D!
    Symbol = fun name -> $@"\det{{{name}}}"
    Declare = @"\det : \mathbb{R}^{n \product n} \to \mathbb{R}"
}
/// In linear algebra, the computation of the permanent of a matrix 
/// is a problem that is thought to be more difficult than the computation 
/// of the determinant of a matrix despite the apparent similarity of the definitions.
/// 
/// The permanent is defined similarly to the determinant, 
/// as a sum of products of sets of matrix entries that lie in distinct rows and columns. 
/// However, where the determinant weights each of these products with a "+/-" sign based on the parity of the set, 
/// the permanent weights them all with a "+" sign.
/// 
/// While the determinant can be computed in polynomial time 
/// by Gaussian elimination, it is generally believed that the permanent cannot be 
/// computed in polynomial time. 
/// In computational complexity theory, a theorem of Valiant states that computing 
/// permanents is #P-hard, and even #P-complete for matrices in which all entries are 0 or 1 Valiant (1979). 
/// 
/// This puts the computation of the permanent in a class of problems believed to be even more difficult to compute than NP. 
/// It is known that computing the permanent is impossible for logspace-uniform ACC0 circuits.(Allender & Gore 1994)
[<CompiledName "PermanentOperator">]
let permanentOperator : MathOperator<double[,], double> = {
    Compute = fun mat -> 
        let rec per (mat: double[,]) : double =
            let n = mat.GetLength(0)
            if n = 1 then mat[0,0]
            elif n = 2 then mat[0,0] * mat[1,1] + mat[0,1] * mat[1,0] // <-- use all chances of inverttions
            else
                /// Takes slice of target minor.
                let getMinor (m: double[,]) row col =
                    let size = m.GetLength(0)
                    Array2D.init (size - 1) (size - 1) (fun i j ->
                        let rowIndex = if i < row then i else i + 1
                        let colIndex = if j < col then j else j + 1
                        m[rowIndex, colIndex])

                [0 .. n - 1]
                |> List.map (fun col ->
                    let minor = getMinor mat 0 col
                    async {
                        let sign = if col % 2 = 0 then 1.0 else -1.0
                        let detMinor = per minor
                        return mat[0, col] * detMinor // <-- sign check question
                    })
                |> Async.Parallel
                |> Async.RunSynchronously
                |> Array.sum
        let result = task {
            let n = mat.GetLength(0)
            if n <= 2 then
                return per mat
            else
                let getMinor (m: double[,]) row col =
                    let size = m.GetLength(0)
                    Array2D.init (size - 1) (size - 1) (fun i j ->
                        let rowIndex = if i < row then i else i + 1
                        let colIndex = if j < col then j else j + 1
                        m[rowIndex, colIndex])

                let tasks =
                    [0 .. n - 1]
                    |> List.map (fun col ->
                        let minor = getMinor mat 0 col
                        Task.Run(fun () ->
                            let sign = if col % 2 = 0 then 1.0 else -1.0
                            sign * mat[0, col] * per minor))

                let! results = Task.WhenAll(tasks)
                return Array.sum results 
        }
        result.Result // :D!
    Symbol = fun name -> $@"\text{{perm}}{name}"
    Declare = @"\text{perm} : \mathbb{R}^{n \product n} \to \mathbb{R}"
}
/// In linear algebra, the trace of a square matrix A, 
/// denoted tr(A) is the sum of the elements on its main diagonal
[<CompiledName "TraceOperator">]
let traceOperator : MathOperator<double[,], double> = {
    Compute = fun mat ->
        let mutable result = 0.0 
        for i = 0 to mat.GetLength 0 do
            result <- result + mat[i, i]

        result
    Symbol = fun name -> $@"\text{{ tr }}{name}"
    Declare = @"\text{tr} : \mathbb{R}^{n \product n} \to \mathbb{R}"
}