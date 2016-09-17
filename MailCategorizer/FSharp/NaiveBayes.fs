namespace MailCategorizer.NaiveBayes

open MailCategorizer

module Classifier = 
    let tokenScore (group:DocsGroup) (token:Token) =
        if group.TokenFrequencies.ContainsKey token
        then log group.TokenFrequencies.[token]
        else 0.0
    let score (document:TokenizedDoc) (group:DocsGroup) =
        let scoreToken = tokenScore group
        log group.Proportion +
            (document |> Seq.sumBy scoreToken)
    let classify (groups:(_*DocsGroup)[])
                 (tokenizer:Tokenizer)
                 (txt:string) =
        let tokenized = tokenizer txt
        groups
        |> Array.maxBy (fun (label,group) -> score tokenized group)
        |> fst

    let learn (docs:(_*string)[])
              (tokenizer:Tokenizer)
              (classificationTokens: Token Set) =
        let total = docs.Length
        docs
        |> Array.map (fun (label, docs) -> label, tokenizer docs)
        |> Seq.groupBy fst
        |> Seq.map(fun (label, group) -> label,group |> Seq.map snd)
        |> Seq.map(fun (label, group) -> label,DocumentGroupAnalyzer.analyze group total classificationTokens)
        |> Seq.toArray

    let train (docs:(_*string)[])
              (tokenizer:Tokenizer)
              (classificationTokens: Token Set) =
        let groups = learn docs tokenizer classificationTokens
        classify groups tokenizer