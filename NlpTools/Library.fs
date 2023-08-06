namespace NlpTools

open System.Collections.Generic
open System
open System.Linq
open System.Text

module CoNLLU =
    type PartOfSpeech = 
    /// ADJ: adjective
    | Adjective
    /// ADP: adposition
    | Adposition
    /// ADV: adverb
    | Adverb
    /// AUX: auxiliary
    | Auxiliary
    /// CCONJ: coordinating conjunction
    | CoordinatingConjunction
    /// DET: determiner
    | Determiner
    /// INTJ: interjection
    | Interjection
    /// NOUN: noun
    | Noun
    /// NUM: numeral
    | Numeral
    /// PART: particle
    | Particle
    /// PRON: pronoun
    | Pronoun
    /// PROPN: proper noun
    | ProperNoun
    /// PUNCT: punctuation
    | Punctuation
    /// SCONJ: subordinating conjunction
    | SubordinatingConjunction
    /// SYM: symbol
    | Symbol
    /// VERB: verb
    | Verb
    /// X: other
    | Other

    type WordId =
    | Position of int
    | Range of int * int
    | NullPosition of int * int

    [<Struct>]
    type Word = 
        { ID: WordId
          Form: string;
          Lemma: string option;
          UniversalPartOfSpeech: PartOfSpeech;
          LanguageSpecificPartOfSpeech: string;
          Features: Dictionary<string, string>; 
          Head: int8;
          DependencyRelation: string;
          Dependencies: string;
          Miscellaneous: string; }

    type Sentence =
        { Words: Word list;
          Comments: Dictionary<string, string>; }

    let private parseFeatures (features:string) =
        let featuresArray = features.Split [| '|' |]
        let result = Dictionary<string, string>()
        if features <> "_" && features <> "" then
            for featureDef in featuresArray do
                let parts = featureDef.Split [| '=' |]
                result.Add(parts[0], parts[1])

        result

    let printFeatures (features: Dictionary<string, string>) =
        let x = features.Select(fun x -> $"{x.Key}={x.Value}")
        String.concat "|" x

    let private parseWord (word:string) =
        let parts = word.Split ([|' '|], StringSplitOptions.RemoveEmptyEntries)
        let wordIdString = parts[0]
        let wordId = match wordIdString.Split [| '-' |] with
                        | [| head ; tail |] -> Range (head |> int, tail |> int)
                        | _ -> match wordIdString.Split [| '.' |] with
                                        | [| head ; tail |] -> NullPosition (head |> int, tail |> int)
                                        | [| head |] -> Position (head |> int)
                                        | _ -> failwith $"Invalid word id '%s{wordIdString}'"
        let parseOptionalString value =
            match value with
            | "_" -> None
            | _ -> Some(value)

        let upos = match defaultArg (Array.tryItem 3 parts) "X" with
                    | "ADJ" -> Adjective
                    | "ADP" -> Adposition
                    | "ADV" -> Adverb
                    | "AUX" -> Auxiliary
                    | "CCONJ" -> CoordinatingConjunction
                    | "DET" -> Determiner
                    | "INTJ" -> Interjection
                    | "NOUN" -> Noun
                    | "NUM" -> Numeral
                    | "PART" -> Particle
                    | "PRON" -> Pronoun
                    | "PROPN" -> ProperNoun
                    | "PUNCT" -> Punctuation
                    | "SCONJ" -> SubordinatingConjunction
                    | "SYM" -> Symbol
                    | "VERB" -> Verb
                    | "X" -> Other
                    | _ -> failwith $"Unknown universal part of speech %s{parts[3]}"

        { ID = wordId
          Form = parts[1]
          Lemma = parseOptionalString parts[2]
          UniversalPartOfSpeech = upos;
          LanguageSpecificPartOfSpeech = defaultArg (Array.tryItem 4 parts) "";
          Features = parseFeatures (defaultArg (Array.tryItem 5 parts) "");
          Head = 0y;
          DependencyRelation = "";
          Dependencies = "";
          Miscellaneous = ""; }

    let parseSentence (sentence: string) = 
        let parts = sentence.Split ([|'\r' ; '\n'|], StringSplitOptions.TrimEntries)
        let mutable words = []
        let comments = Dictionary<string, string>()
        for p in parts do
            match p with
            | "" -> ()
            | line when line.StartsWith('#') -> comments.Add(line.Substring(1), "")
            | _ -> 
                let word = parseWord p
                words <- words @ [word]
                ()
        { Words = words; Comments = Dictionary<string, string>() }

    let printWord word =
        let wordId = match word.ID with
                        | Range (start,finish) -> sprintf "%d-%d" start finish
                        | Position pos -> pos |> string
                        | NullPosition (start,finish) -> sprintf "%d.%d" start finish

        let upos = match word.UniversalPartOfSpeech with
                    | Adjective -> "ADJ"
                    | Adposition -> "ADP"
                    | Adverb -> "ADV"
                    | Auxiliary -> "AUX"
                    | CoordinatingConjunction -> "CCONJ"
                    | Determiner -> "DET"
                    | Interjection -> "INTJ"
                    | Noun -> "NOUN"
                    | Numeral -> "NUM"
                    | Particle -> "PART"
                    | Pronoun ->"PRON"
                    | ProperNoun -> "PROPN"
                    | Punctuation ->"PUNCT"
                    | SubordinatingConjunction -> "SCONJ"
                    | Symbol -> "SYM"
                    | Verb -> "VERB"
                    | Other -> "X"

        printf "%-7s" wordId
        printf "%-10s" word.Form
        printf "%-10s" (defaultArg word.Lemma "_")
        printf "%-10s" upos
        printf "%-20s" word.LanguageSpecificPartOfSpeech
        printf "%-20s" (printFeatures word.Features)
        printfn ""

    let printSentence sentence =
        for comment in sentence.Comments do
            printfn "# %s = %s" comment.Key comment.Value
        for word in sentence.Words do
            printWord word

    let reconstructSentence sentence =
        let mutable last = 0
        let result = StringBuilder()
        for w in sentence.Words do
            let wordId = w.ID
            match wordId with
            | Range (_, finish) ->
                last <- finish
                if result.Length <> 0 then 
                    result.Append(" ") |> ignore
                result.Append(w.Form) |> ignore
            | Position p ->
                if p > last then
                    last <- p
                    if result.Length <> 0 then 
                        result.Append(" ") |> ignore
                    result.Append(w.Form) |> ignore
            | _ -> ()
        result.ToString()