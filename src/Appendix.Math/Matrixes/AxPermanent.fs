namespace Appendix.Math.Matrixes

open Appendix.Math.Base    
open System.Threading.Tasks

[<Class>]
type AxPermanentOperator(d: double[,]) =
    inherit AxAbstractOperator<double[,], double>()
    let rec per (matrix: double[,]) : double =
        let n = matrix.GetLength(0)
        if n = 1 then matrix[0,0]
        elif n = 2 then matrix[0,0] * matrix[1,1] - matrix[0,1] * matrix[1,0]
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
    override this.latex(name: string) =
        $@"\text{{per}}{name} = {this.get}"
    override this.get = _result.Result
    override this.abstractDecl =
        $@"\text{{per}} : L \longmapsto \mathbb{{R}}"
    