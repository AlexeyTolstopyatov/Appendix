namespace Appendix.Math.Matrixes

open Appendix.Math.Base    
open System.Threading.Tasks

[<Class>]
type AxPermanentOperator(d: double[,]) =
    inherit AxAbstractOperator<double[,], double>()
    let rec per (matrix: double[,]) : double =
        let n = matrix.GetLength(0)
        if n = 1 then matrix[0,0]
        elif n = 2 then matrix[0,0] * matrix[1,1] + matrix[0,1] * matrix[1,0]
        else
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
                    let detMinor = per minor
                    return sign * matrix[0, col] * detMinor
                })
            |> Async.Parallel
            |> Async.RunSynchronously
            |> Array.sum
    let perAsync (matrix: double[,]) : Task<double> =
        task {
            let n = matrix.GetLength(0)
            if n <= 2 then
                return per matrix
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
                            matrix[0, col] * per minor))

                let! results = Task.WhenAll(tasks)
                return Array.sum results
        }
    let mutable _result: Task<double> = perAsync(d)
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
    override this.latex(name: string) =
        $@"\text{{per}}{name} = {this.get}"
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
    override this.get = _result.Result
    /// Prints declaration of current linear operator
    override this.abstractDecl =
        $@"\text{{per}} : L \longmapsto \mathbb{{R}}"
    