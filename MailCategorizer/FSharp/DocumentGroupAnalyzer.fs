namespace MailCategorizer

module DocumentGroupAnalyzer =
    let proportion count total = float count / float total
    let laplace count total = proportion (count + 1) (total + 1)
    let countIn (group:TokenizedDoc seq) (token:Token) =
        group
        |> Seq.filter (Set.contains token)
        |> Seq.length
    let analyze (group:TokenizedDoc seq)
                (totalDocs:int)
                (classificationTokens:Token Set) =
        let groupSize = group |> Seq.length
        let score token = 
            let count = countIn group token
            laplace count groupSize
        let scoredTokens =
            classificationTokens
            |> Set.map (fun token -> token, score token)
            |> Map.ofSeq
        let groupProportion = proportion groupSize totalDocs

        {
            Proportion = groupProportion
            TokenFrequencies = scoredTokens
        }
 