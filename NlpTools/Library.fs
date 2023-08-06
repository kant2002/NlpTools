namespace NlpTools

open System.Collections.Generic
open System

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

    let parseWord (word:string) =
        let parts = word.Split ([|' '|], StringSplitOptions.RemoveEmptyEntries)
        let wordIdString = parts[0]
        let wordId = match wordIdString.Split [| '-' |] with
                        | [| head ; tail |] -> Range (head |> int, tail |> int)
                        | [| head |] -> Position (head |> int)
                        | _ -> failwith $"Invalid word id '{wordIdString}'"
        let parseOptionalString value =
            match value with
            | "_" -> None
            | _ -> Some(value)

        { ID = wordId
          Form = parts[1]
          Lemma = parseOptionalString parts[2]
          UniversalPartOfSpeech = Adjective;
          LanguageSpecificPartOfSpeech = "";
          Features = Dictionary<string, string>();
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

        printf "%-7s" wordId
        printf "%-10s" word.Form
        printf "%-10s" (defaultArg word.Lemma "_")
        printfn ""

    let printSentence sentence =
        for comment in sentence.Comments do
            printfn "# %s = %s" comment.Key comment.Value
        for word in sentence.Words do
            printWord word