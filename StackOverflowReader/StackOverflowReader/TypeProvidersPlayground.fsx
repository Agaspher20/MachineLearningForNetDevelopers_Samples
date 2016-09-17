
#I @"..\packages\"
#r @"FSharp.Data.2.3.2\lib\net40\FSharp.Data.dll"

open FSharp.Data 

[<Literal>]
let stackOverflowPath = """http://api.stackexchange.com/2.2/questions?site=stackoverflow"""

let csQuestionsPath = stackOverflowPath + """&order=desc&sort=activity&tagged=CS"""
type Questions = JsonProvider<stackOverflowPath>

Questions.Load(csQuestionsPath).Items |> Seq.iter (fun q -> printfn "%s" q.Title)
[<Literal>]
let sample = """
    {
        "items": [
            {
                "tags":["java","arrays"],
                "owner": ""
            },
            {
                "tags":["javascript","jquery","html"],
                "owner": ""
            }],
        "has_more":true,
        "quota_max":300,
        "quota_remaining":299
    }""" 
let javaQuestionsPath = stackOverflowPath + """&order=desc&sort=activity&tagged=java"""

type HardCodedQuestions = JsonProvider<sample>

let tagged tags query =
    // join the tags in a ; separated string
    let joinedTags = tags |> String.concat ";"
    sprintf "%s&tagged=%s" query joinedTags 
let page p query = sprintf "%s&page=%i" query p 
let pageSize s query = sprintf "%s&pagesize=%i" query s 
let extractQuestions (query:string) = Questions.Load(query).Items

let CS = "C%23"
let FS = "F%23"

let fsSample =
    stackOverflowPath
    |> tagged [FS]
    |> pageSize 100
    |> extractQuestions

let csSample =
    stackOverflowPath
    |> tagged [CS]
    |> pageSize 100
    |> extractQuestions


let analyzeTags (qs:Questions.Item seq) =
    qs
    |> Seq.collect (fun question -> question.Tags)
    |> Seq.countBy id
    |> Seq.filter (fun (_,count) -> count > 2)
    |> Seq.sortBy (fun (_,count) -> -count)
    |> Seq.iter (fun (tag,count) -> printfn "%s,%i" tag count)

analyzeTags fsSample
analyzeTags csSample
