open System.IO

type DocType =
    | Ham
    | Spam

let generalPath = __SOURCE_DIRECTORY__ + @"..\..\data\";

let dataSetPath = generalPath + "SMSSpamCollection.txt"

let identify (example:DocType*string) =
    let docType,content = example
    match docType with
    | Ham -> printfn "'%s' is ham" content
    | Spam -> printfn "'%s' is spam" content

let parseDocType (label:string) =
    match label.ToLower() with
    | "ham" -> Ham
    | "spam" -> Spam
    | _ -> failwith "Unknown label"

let parseLine(textData:string) =
    let values = textData.Split('\t')
    (values.[0] |> parseDocType, values.[1])

let dataSet =
    File.ReadAllLines dataSetPath
    |> Array.map parseLine

open System.Text.RegularExpressions

let matchWords = Regex(@"\w+")

let training = dataSet.[1001..]
let validation = dataSet.[0..1000]

#load "Model.fs"
#load "DocumentGroupAnalyzer.fs"
#load "NaiveBayes.fs"
open MailCategorizer
open MailCategorizer.NaiveBayes

let evaluate (tokenizer:Tokenizer) (tokens:Token Set) (tokenizerName:string) =
    let classifier = Classifier.train training tokenizer tokens
    validation
        |> Seq.averageBy (fun (docType,sms) ->
            if docType = classifier sms then 1.0 else 0.0)
        |> fun x -> x * 100.0
        |> printfn "%s correctly classified: %.1f" tokenizerName

let buildVocabulary (tokenizer:Tokenizer) (texts: (DocType*string)[]) =
    texts
    |> Seq.map snd
    |> Seq.map tokenizer
    |> Set.unionMany

let tokenizer (text:string) =
    text.ToLowerInvariant()
    |> matchWords.Matches
    |> Seq.cast<Match>
    |> Seq.map (fun m -> m.Value)
    |> Set.ofSeq

let tokenSet = buildVocabulary tokenizer training

let casedTokenizer (text:string) =
    text
    |> matchWords.Matches
    |> Seq.cast<Match>
    |> Seq.map (fun m -> m.Value)
    |> Set.ofSeq 

let casedTokenSet = buildVocabulary casedTokenizer training

let hardCodedTokenSet = ["txt"] |> set

let top n (tokenizer:Tokenizer) (docs:string []) =
    let tokenized =
        docs
        |> Array.map tokenizer
    tokenized
    |> Set.unionMany
    |> Seq.sortBy (fun t -> - DocumentGroupAnalyzer.countIn tokenized t)
    |> Seq.take n
    |> Set.ofSeq

let ham,spam =
    let rawHam,rawSpam =
        training
        |> Array.partition (fun (lbl,_) -> lbl=Ham)
    
    rawHam, rawSpam

let tokenizedHam = ham |> buildVocabulary casedTokenizer
let tokenizedSpam = spam |> buildVocabulary casedTokenizer
let hamCount = tokenizedHam |> Set.count
let spamCount = tokenizedSpam |> Set.count
let topHam = ham |> Array.map snd |> top (hamCount / 10) casedTokenizer
let topSpam = spam |> Array.map snd |> top (spamCount / 10) casedTokenizer 
let topTokens = Set.union topHam topSpam

let commonTokens = Set.intersect topHam topSpam
let differentTokens = Set.difference topTokens commonTokens

let rareTokens n (tokenizer:Tokenizer) (docs:string []) =
    let tokenized = docs |> Array.map tokenizer

    tokenized
    |> Set.unionMany
    |> Seq.sortBy (fun t -> DocumentGroupAnalyzer.countIn tokenized t)
    |> Seq.take n
    |> Set.ofSeq 
let rareHam = ham |> Array.map snd |> rareTokens 50 casedTokenizer |> Seq.iter (printfn "%s")

let phoneToken = "__PHONE__"
let txtToken = "__TXT__"
let phoneWords = Regex(@"0[7-9]\d{9}")
let phone (text:string) =
    match (phoneWords.IsMatch text) with
    | true -> phoneToken
    | false -> text 
let txtCode = Regex(@"\b\d{5}\b")
let txt (text:string) =
    match (txtCode.IsMatch text) with
    | true -> txtToken
    | false -> text 
let smartTokenizer = casedTokenizer >> Set.map phone >> Set.map txt

let smartTokens = differentTokens |> Set.add phoneToken |> Set.add txtToken

let lengthAnalysis len = 
    let long (msg:string) = msg.Length > len
    let ham,spam =
        dataSet
        |> Array.partition (fun (docType,_) -> docType = Ham)
    let spamAndLongCount =
        spam
        |> Array.filter (fun (_,sms) -> long sms)
        |> Array.length
    let longCount =
        dataSet
        |> Array.filter (fun (_,sms) -> long sms)
        |> Array.length 
    let pSpam = (float spam.Length) / (float dataSet.Length)
    let pLongIfSpam = float spamAndLongCount / float spam.Length
    let pLong = float longCount / float (dataSet.Length) 
    let pSpamIfLong = pLongIfSpam * pSpam / pLong
    pSpamIfLong 

for l in 10 .. 10 .. 130 do
    printfn "P(Spam if Length > %i) = %.4f" l (lengthAnalysis l)

let longMessageToken = "___LONG___"
let shortMessageToken = "___SHORT___"
let smartLengthTokenizer (text:string) = 
    smartTokenizer text |> Set.add (if text.Length > 120 then longMessageToken else shortMessageToken)
let smartLengthTokens = smartTokens |> Set.add longMessageToken |> Set.add shortMessageToken

evaluate tokenizer tokenSet "Full token set"
evaluate casedTokenizer tokenSet "Cased token set"
evaluate tokenizer hardCodedTokenSet "Only hard coded token 'txt'"
evaluate casedTokenizer topTokens "Top 10% tokens from each set (Top tokens)"
evaluate casedTokenizer differentTokens "Top tokens with similar tokens excluded (Different tokens)"
evaluate smartTokenizer smartTokens "Different tokens with phones and txts (smart tokens)"
evaluate smartLengthTokenizer smartLengthTokens "Smart tokens with length flag"

let bestClassifier = Classifier.train training smartLengthTokenizer smartLengthTokens
let classifiedScore (docType, sms) =
    if docType = bestClassifier sms
    then 1.0
    else 0.0
validation
|> Seq.filter (fun (docType,_) -> docType = Ham)
|> Seq.averageBy classifiedScore
|> printfn "Properly classified Ham: %.5f"
validation
|> Seq.filter (fun (docType,_) -> docType = Spam)
|> Seq.averageBy classifiedScore
|> printfn "Properly classified Spam: %.5f"