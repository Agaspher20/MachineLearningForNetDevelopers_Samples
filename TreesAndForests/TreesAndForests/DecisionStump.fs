module DecisionStump

    let mostFrequentLabelIn group =
        group
        |> Seq.countBy snd
        |> Seq.maxBy snd
        |> fst

    let hasData extractFeature = extractFeature >> Option.isSome
    
    let learn sample extractFeature extractLabel =
        // group together observations that have the
        // same value for the selected feature, and
        // find the most frequent label by group.
        let groups =
            sample
            |> Seq.map (fun obs -> extractFeature obs, extractLabel obs)
            |> Seq.groupBy fst
            |> Seq.map (fun (feat,group) -> feat, mostFrequentLabelIn group)
        // for an observation, find the group with
        // matching feature value, and predict the
        // most frequent label for that group.
        let classifier obs =
            let featureValue = extractFeature obs
            groups
            |> Seq.find (fun (f,_) -> f = featureValue)
            |> snd

        classifier

    let betterLearn sample extractFeature extractLabel =
        let branches =
            sample
            |> Seq.filter (extractFeature |> hasData)
            |> Seq.map (fun obs -> extractFeature obs |> Option.get, extractLabel obs)
            |> Seq.groupBy fst
            |> Seq.map (fun (feat,group) -> feat, mostFrequentLabelIn group)
            |> Map.ofSeq
        let labelForMissingValues =
            sample
            |> Seq.countBy extractLabel
            |> Seq.maxBy snd
            |> fst
        let classifier obs =
            let featureValue = extractFeature obs
            match featureValue with
            | None -> labelForMissingValues
            | Some(value) ->
                match (branches.TryFind value) with
                | None -> labelForMissingValues
                | Some(predictedLabel) -> predictedLabel
        classifier

    let entropy data =
        let size = data |> Seq.length

        data
        |> Seq.countBy id
        |> Seq.map (fun (_,count) -> float count / float size)
        |> Seq.sumBy (fun f -> if f > 0. then - f * log f else 0.)

    let splitEntropy extractLabel extractFeature data =
        // observations with no missing values
        // for the selected feature
        let dataWithValues =
            data
            |> Seq.filter (extractFeature |> hasData)
        let size = dataWithValues |> Seq.length
        dataWithValues
        |> Seq.groupBy extractFeature
        |> Seq.sumBy (fun (_,group) ->
            let groupSize = group |> Seq.length
            let probaGroup = float groupSize / float size
            let groupEntropy = group |> Seq.map extractLabel |> entropy
            probaGroup * groupEntropy)
