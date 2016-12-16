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
