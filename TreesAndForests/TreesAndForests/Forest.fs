module Forest
    open System
    open Tree

    let pickRepeat (rng:Random) proportion original =
        let size = original |> Array.length
        let sampleSize = proportion * float size |> int
        Array.init sampleSize (fun _ -> original.[rng.Next(size)])

    let pickNoRepeat (rng:Random) proportion original =
        let size = original |> Seq.length
        let sampleSize = proportion * float size |> int
        let init = ([],size)
        original
        |> Seq.fold (fun (sampled,remaining) item ->
            let picked = sampled |> List.length
            let p = float (sampleSize - picked) / float remaining
            if (rng.NextDouble () <= p)
            then (item::sampled,remaining-1)
            else (sampled,remaining-1)) init
        |> fst

    let predict forest observation =
        forest
        |> Seq.map (fun tree -> decide tree observation)
        |> Seq.countBy id
        |> Seq.maxBy snd
        |> fst

    let growForest size sample label features =
        let rng = Random ()
        let propFeatures =
            let total = features |> Seq.length |> float
            sqrt total / total 
        let featSample () = pickNoRepeat rng propFeatures features
        let popSample () = pickRepeat rng 1.0 sample
        let filters = [ leafSizeFilter 10; entropyGainFilter ] 
        let forest = [
            for _ in 1 .. size ->
                let sample = popSample ()
                let features = featSample () |> Map.ofList
                growTree filters sample label features ]
        let predictor = predict forest
        predictor
