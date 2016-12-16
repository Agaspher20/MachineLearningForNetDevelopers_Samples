module Tree
    open DecisionStump
    type Feature<'a> = 'a -> string Option
    type NamedFeature<'a> = string * Feature<'a>
    type Label<'a, 'b when 'b:equality> = 'a -> 'b
    type Tree<'a, 'b when 'b:equality> =
        | Answer of 'b
        | Stump of NamedFeature<'a> * string * Map<string,Tree<'a,'b>>

    let rec decide tree observation =
        match tree with
        | Answer(labelValue) -> labelValue
        | Stump((featureName,feature),valueWhenMissing,branches) ->
            let featureValue = feature observation
            let usedValue =
                match featureValue with
                | None -> valueWhenMissing
                | Some(value) ->
                    match (branches.TryFind value) with
                    | None -> valueWhenMissing
                    | Some(_) -> value
            let nextLevelTree = branches.[usedValue]
            decide nextLevelTree observation

    let mostFrequentBy f sample = 
        sample
        |> Seq.map f
        |> Seq.countBy id
        |> Seq.maxBy snd
        |> fst

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

    let rec growTree sample label features =
        if (Map.isEmpty features)
        // we have no feature left to split on:
        // our prediction is the most frequent
        // label in the dataset
        then sample |> mostFrequentBy label |> Answer
        else
            // from the named features we have available,
            // identify the one with largest entropy gain.
            let (bestName, bestFeature) =
                features
                |> Seq.minBy (fun kv ->
                    splitEntropy label kv.Value sample)
                |> (fun kv -> kv.Key, kv.Value)
            // create a group for each of the values the
            // feature takes, eliminating the cases where
            // the value is missing.
            let branches =
                sample
                |> Seq.groupBy bestFeature
                |> Seq.filter(fun (value,group) -> value.IsSome)
                |> Seq.map(fun (value,group) -> value.Value,group)
            // find the most frequent value for the feature;
            // we'll use it as a default replacement for missing values.
            let defaultValue =
                branches
                |> Seq.maxBy (fun (value,group) ->
                    group |> Seq.length)
                |> fst
            // remove the feature we selected from the list of
            // features we can use at the next tree level
            let remainingFeatures = features |> Map.remove bestName
            // ... and repeat the operation for each branch,
            // building one more level of depth in the tree
            let nextLevel =
                branches
                |> Seq.map (fun (value,group) ->
                    value, growTree group label remainingFeatures)
                |> Map.ofSeq

            Stump((bestName,bestFeature), defaultValue, nextLevel)