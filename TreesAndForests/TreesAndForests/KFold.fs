module KFold

    let kfold k sample = 
        let size = sample |> Array.length
        let foldSize = size / k
        [ for f in 0 .. (k-1) do
            let sliceStart = f * foldSize
            let sliceEnd = f * foldSize + foldSize - 1
            let validation = sample.[sliceStart..sliceEnd]
            let training =
                [|
                    for i in 0 .. (sliceStart - 1) do yield sample.[i]
                    for i in (sliceEnd + 1) .. (size - 1) do yield sample.[i]
                |]
            yield training,validation
        ]
