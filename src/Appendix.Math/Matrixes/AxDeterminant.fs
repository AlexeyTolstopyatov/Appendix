namespace Appendix.Math.Matrixes
open System.Threading.Tasks
open Appendix.Math.Base
///
/// This module contains matrix determinant
/// methods for common fractions and f64 types
///
/// Sources was taken and reproduced from "Esox"
/// (see Esox-Math GitHub repo).
///
[<Class>]
type AxDeterminantOperator(d: double[,]) =
    inherit AxAbstractOperator<double[,], double>()
    let rec det (matrix: double[,]) : double =
        let n = matrix.GetLength(0)
        if n = 1 then matrix[0,0]
        elif n = 2 then matrix[0,0] * matrix[1,1] - matrix[0,1] * matrix[1,0]
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
                let minor = getMinor matrix 0 col
                async {
                    let sign = if col % 2 = 0 then 1.0 else -1.0
                    let detMinor = det minor
                    return sign * matrix[0, col] * detMinor
                })
            |> Async.Parallel
            |> Async.RunSynchronously
            |> Array.sum

    let detAsync (matrix: double[,]) : Task<double> =
        task {
            let n = matrix.GetLength(0)
            if n <= 2 then
                return det matrix
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
                        let minor = getMinor matrix 0 col
                        Task.Run(fun () ->
                            let sign = if col % 2 = 0 then 1.0 else -1.0
                            sign * matrix[0, col] * det minor))

                let! results = Task.WhenAll(tasks)
                return Array.sum results
        }
    let mutable _result: Task<double> = detAsync(d)
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
    override _.get =
        _result.Result
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
    override this.latex(obj: string) =
        $@"\det{{{obj}}} = {this.get}"
    /// Prints the declaration of current linear operator
    override this.abstractDecl =
        @"\det : L \longmapsto \mathbb{R}"