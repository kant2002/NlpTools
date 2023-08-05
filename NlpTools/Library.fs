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

    [<Struct>]
    type Word = 
        { ID: string
          Form: string;
          Lemma: string;
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
        { ID = parts[0]
          Form = parts[1]
          Lemma = parts[2]
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
        printfn "%-7s%-10s%-10s" word.ID word.Form word.Lemma

    let printSentence sentence =
        for comment in sentence.Comments do
            printfn "# %s = %s" comment.Key comment.Value
        for word in sentence.Words do
            printWord word