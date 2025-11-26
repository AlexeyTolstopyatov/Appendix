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

                let! results = Task.WhenAll tasks
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
/// Nullity (kernel set) resolving in this library is
/// a step-by-step solution.
type KernelStep =
    | InitialMatrix of double[,]
    | RowReduction of double[,] * string
    | BasisIdentification of double[][] * string[]
    | FinalBasis of double[][]

type KernelSolution = {
    Basis: double[][]
    Nullity: int
    FreeVariables: string[]
}

let private matrixToString (matrix: double[,]) = 
    let rows = matrix.GetLength(0)
    let cols = matrix.GetLength(1)
    let elements = 
        [ for i in 0..rows-1 do
            [ for j in 0..cols-1 -> sprintf "%.2f" matrix.[i,j] ]
            |> String.concat " & "
        ]
    $"""\begin{{pmatrix}}{String.concat " \\\\\n" elements}\end{{pmatrix}}"""

let private vectorToString (vector: double[]) =
    let elements = vector |> Array.map (sprintf "%.2f") |> String.concat " \\\\ "
    $"""\begin{{pmatrix}} {elements} \end{{pmatrix}}"""

/// Rows echelon
let private makeRowsEchelon (matrix: double[,]) : double[,] * int[] =
    let epsilon = 1e-10
    let m = matrix.GetLength 0
    let n = matrix.GetLength 1
    let mat = Array2D.copy matrix
    let pivotCols = ResizeArray<int>()
    let mutable r = 0
    
    for c in 0..n-1 do
        if r >= m then () else
        // Find zeroed element
        let mutable pivotRow = r
        while pivotRow < m && abs(mat.[pivotRow, c]) < epsilon do
            pivotRow <- pivotRow + 1
        
        if pivotRow < m then
            // replace rows
            if pivotRow <> r then
                for j in 0..n-1 do
                    let temp = mat.[pivotRow, j]
                    mat.[pivotRow, j] <- mat.[r, j]
                    mat.[r, j] <- temp
            
            let pivot = mat.[r, c]
            for j in 0..n-1 do
                mat.[r, j] <- mat.[r, j] / pivot
            
            // make other members --> zeroes
            for i in 0..m-1 do
                if i <> r then
                    let factor = mat.[i, c]
                    for j in 0..n-1 do
                        mat.[i, j] <- mat.[i, j] - factor * mat.[r, j]
            
            pivotCols.Add(c)
            r <- r + 1
    
    mat, pivotCols.ToArray()

let private computeKernel (matrix: double[,]) : KernelStep list * KernelSolution =
    let m = matrix.GetLength 0
    let n = matrix.GetLength 1

    let rowsEchelon, pivotCols = makeRowsEchelon matrix
    
    // variables what are will be freed 
    let pivotSet = Set.ofArray pivotCols
    let freeCols = 
        [0..n-1] 
        |> List.filter (fun j -> not (pivotSet.Contains j))
    
    // basis?
    // little warning: I want to see academic look at the situation.
    // epsilon in this function strongly fixed and equal 10^(-5).
    let basis =
        freeCols
        |> List.map (fun freeCol ->
            let vector = Array.zeroCreate n
            vector.[freeCol] <- 1.0  // free var := 1
            
            // x_1 = t_1 + 2t_2 + ... + nt_n 
            let mutable currentPivot = 0
            for row in 0..m-1 do
                // skip zeroed rows
                let mutable isZeroRow = true
                for col in 0..n-1 do
                    if abs(rowsEchelon.[row, col]) > 1e-5 then
                        isZeroRow <- false
                        // break;        

                if isZeroRow then () else
                
                let pivotCol = pivotCols.[currentPivot]
                vector.[pivotCol] <- -rowsEchelon.[row, freeCol]
                currentPivot <- currentPivot + 1
            vector
        )
        |> List.toArray
    
    // Names of free variables in system
    let freeVars = freeCols 
                    |> List.mapi (fun i _ -> $"t_{i + 1}") 
                    |> List.toArray
    
    let solution = {
        Basis = basis
        Nullity = basis.Length
        FreeVariables = freeVars
    }
    
    // define steps
    let steps = [
        InitialMatrix matrix
        RowReduction (rowsEchelon, "Current reduced rows echelon")
        BasisIdentification (basis, freeVars)
        FinalBasis basis
    ]
    
    steps, solution
/// In mathematics, the kernel of a linear map, also known as the null space or nullspace, 
/// is the part of the domain which is mapped to the zero vector of the co-domain; 
/// the kernel is always a linear subspace of the domain. That is, given a linear map L : V → W 
/// between two vector spaces V and W, the kernel of L is the vector space of all elements v of V 
/// such that L(v) = 0, where 0 denotes the zero vector in W, or more symbolically:
///     Ker(A) = {x | A*x = 0}
[<CompiledName "KernelOperator">]
let kernelOperator : SymbolicMathOperator<double[,], KernelStep, KernelSolution> = {
    Steps = fun matrix -> 
        let steps, _ = computeKernel matrix
        steps
    
    Result = fun steps ->
        let basis = 
            match steps |> List.last with
            | FinalBasis b -> b
            | _ -> failwith "Last step is not FinalBasis"
        let freeVars =
            steps
            |> List.tryPick (function 
                | BasisIdentification (_, freeVars) -> Some freeVars
                | _ -> None)
            |> Option.defaultWith (fun () -> [||])

        { Basis = basis; Nullity = basis.Length; FreeVariables = freeVars }
    
    ToString = fun steps name ->
        let stepTexts = 
            steps 
            |> List.map (fun step ->
                match step with
                | InitialMatrix m -> $@"{name} = {matrixToString m}"
                | RowReduction (m, desc) -> $@"\text{{ {desc} }} {matrixToString m}"
                | BasisIdentification (vectors, vars) ->
                    let basisText = 
                        vectors 
                        |> Array.mapi (fun i v ->
                            $@"c_{i + 1} = {vectorToString v}")
                        |> String.concat ",\\quad "
                    $"\\text{{Basis of {name} }}: \n{basisText}"
                | FinalBasis basis ->
                    let basisLatex = 
                        basis 
                        |> Array.mapi (fun i vec ->
                            $@"t_{{{i + 1}}} \cdot {vectorToString vec}")
                        |> String.concat " + "
                    $@"\ker {name} = {basisLatex}, t_i \in \mathbb{{R}}"
            )
        String.concat " \\\\ \n" stepTexts
    
    Declare = @"\ker : \mathbb{R}^{m \times n} \to \mathbb{R}^n"
}

type ImageStep =
    | InitialMatrix of double[,]
    | ColumnSpace of double[][]
    | BasisExtraction of double[][]
    | FinalImageBasis of double[][]

type ImageSolution = {
    Basis: double[][]
    Rank: int
    Dimension: int
}
let private vectorsToString (vectors: double[][]) =
    vectors 
    |> Array.mapi (fun i vec -> $@"c_{i + 1} = {vectorToString vec}")
    |> String.concat ",\\quad "
/// Image (also known as range) is a concept in linear algebra 
/// that refers to the set of all vectors that are the result 
/// of a linear transformation (mapping). 
/// 
/// This concept is related to the fact that a linear transformation 
/// preserves linear combinations of vectors, meaning it preserves the way 
/// vectors are added and multiplied by scalars 
/// from the original vector to the transformed vector.
[<CompiledName "ImageOperator">]
let imageOperator : SymbolicMathOperator<double[,], ImageStep, ImageSolution> = {
    Steps = fun matrix ->
        let m = matrix.GetLength(0)
        
        let rrefMatrix, pivotCols = makeRowsEchelon matrix
        
        let basis = pivotCols
                    |> Array.map (fun colIdx ->
                        Array.init m (fun row -> matrix.[row, colIdx])
                    )
        
        [
            InitialMatrix matrix
            ColumnSpace basis
            BasisExtraction basis
            FinalImageBasis basis
        ]
    
    Result = fun steps ->
        let finalStep = steps |> List.last
        match finalStep with
        | FinalImageBasis basis ->
            { 
                Basis = basis
                Rank = basis.Length
                Dimension = basis.Length
            }
        | _ -> failwith "Invalid steps"
    
    ToString = fun steps name ->
        let stepTexts =
            steps 
            |> List.map (fun step ->
                match step with
                | InitialMatrix m -> 
                    $@"{name} = {matrixToString m}"
                
                | ColumnSpace basis ->
                    $@"\text{{Columns set: }} {vectorsToString basis}"
                
                | BasisExtraction basis ->
                    let basisText = vectorsToString basis
                    $@"\text{{Image's basis: }} {basisText}"
                
                | FinalImageBasis basis ->
                    let basisLatex = 
                        basis 
                        |> Array.mapi (fun i vec -> $@"s_{{{i + 1}}} \cdot {vectorToString vec}")
                        |> String.concat " + "
                    $@"\text{{im}} {name} = {basisLatex}, s_i \in \mathbb{{R}}"
            )

        String.concat " \\\\ \n" stepTexts
    
    Declare = 
        @"\text{im} : \mathbb{R}^{m \times n} \to \mathbb{R}^m"
}