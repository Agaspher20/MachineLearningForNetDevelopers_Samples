module PrincipalComponentAnalysis

    open MathNet
    open MathNet.Numerics.LinearAlgebra
    open MathNet.Numerics.Statistics

    let buildCovarianceMatrix (originalMatrix:Matrix<float>) =
        let columnsCount = originalMatrix.ColumnCount
        let covarianceMatrix = DenseMatrix.create columnsCount columnsCount Matrix.Zero
        for c1 in 0 .. (columnsCount - 1) do
            covarianceMatrix.[c1,c1] <- Statistics.Variance (originalMatrix.Column c1)
            for c2 in (c1 + 1) .. (columnsCount - 1) do
                let covariance = Statistics.Covariance (originalMatrix.Column c1, originalMatrix.Column c2)
                covarianceMatrix.[c1,c2] <- covariance
                covarianceMatrix.[c2,c1] <- covariance
        covarianceMatrix

    let normalize dim (observations:float[][]) = 
        let averages =
            Array.init dim (fun i ->
                observations
                |> Seq.averageBy (fun x -> x.[i])) 
        let stdDevs =
            Array.init dim (fun i ->
                let avg = averages.[i]
                observations
                |> Seq.averageBy (fun x ->
                    pown (float x.[i] - avg) 2 |> sqrt))
        observations
        |> Array.map (fun row ->
            row
            |> Array.mapi (fun i x ->
                (float x - averages.[i]) / stdDevs.[i]))
                
    let principalComponentAnalysis (observations:float[][]) =
        let factorization =
            observations
            |> Matrix.Build.DenseOfRowArrays
            |> buildCovarianceMatrix
            |> Matrix.eigen
        let eigenValues = factorization.EigenValues
        let eigenVectors = factorization.EigenVectors 
        let projector (observation:float[]) =
            let observationVector = observation |> Vector.Build.DenseOfArray
            (eigenVectors.Transpose () * observationVector)
            |> Vector.toArray 
        (eigenValues,eigenVectors), projector
