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
          UniversalPartOfSpeech: PartOfSpeech option;
          LanguageSpecificPartOfSpeech: string option;
          Features: Dictionary<string, string>; 
          Head: uint8 option;
          DependencyRelation: string option;
          Dependencies: string option;
          Miscellaneous: Dictionary<string, string>; }

    type Sentence =
        { Words: List<Word>;
          Comments: Dictionary<string, string>; }

    let private parseDictionary (features:string) =
        let featuresArray = features.Split [| '|' |]
        let result = Dictionary<string, string>()
        if features <> "" then
            for featureDef in featuresArray do
                if featureDef <> "_" then
                    let parts = featureDef.Split [| '=' |]
                    result.Add(parts[0], parts[1])

        result

    let private printDictionary (features: Dictionary<string, string>) =
        let x = features.Select(fun x -> $"{x.Key}={x.Value}")
        String.concat "|" x

    let private parseOptionalString value =
        match value with
        | "_" -> None
        | _ -> Some(value)

    let private isUpos posString =
        match posString with
        | "ADJ"   -> true
        | "ADP"   -> true
        | "ADV"   -> true
        | "AUX"   -> true
        | "CCONJ" -> true
        | "DET"   -> true
        | "INTJ"  -> true
        | "NOUN"  -> true
        | "NUM"   -> true
        | "PART"  -> true
        | "PRON"  -> true
        | "PROPN" -> true
        | "PUNCT" -> true
        | "SCONJ" -> true
        | "SYM"   -> true
        | "VERB"  -> true
        | "X"     -> true
        | "_"     -> true
        | _ -> false

    let private parseUpos posString =
        match posString with
        | Some pos ->
            match pos with
            | "ADJ"   -> Some(Adjective)
            | "ADP"   -> Some(Adposition)
            | "ADV"   -> Some(Adverb)
            | "AUX"   -> Some(Auxiliary)
            | "CCONJ" -> Some(CoordinatingConjunction)
            | "DET"   -> Some(Determiner)
            | "INTJ"  -> Some(Interjection)
            | "NOUN"  -> Some(Noun)
            | "NUM"   -> Some(Numeral)
            | "PART"  -> Some(Particle)
            | "PRON"  -> Some(Pronoun)
            | "PROPN" -> Some(ProperNoun)
            | "PUNCT" -> Some(Punctuation)
            | "SCONJ" -> Some(SubordinatingConjunction)
            | "SYM"   -> Some(Symbol)
            | "VERB"  -> Some(Verb)
            | "X"     -> Some(Other)
            | "_"     -> None
            | _ -> failwith $"Unknown universal part of speech %s{pos}"
        | None -> None
    
    let private parseString v =
        match v with
        | Some v when v = "_" -> None
        | None -> None
        | _ -> v

    let private parseWord (word:string) =
        let parts = word.Split ([| '\t' |], StringSplitOptions.RemoveEmptyEntries)
        let wordIdString = parts[0]
        let wordId = match wordIdString.Split [| '-' |] with
                        | [| head ; tail |] -> Range (head |> int, tail |> int)
                        | _ -> match wordIdString.Split [| '.' |] with
                                        | [| head ; tail |] -> NullPosition (head |> int, tail |> int)
                                        | [| head |] -> Position (head |> int)
                                        | _ -> failwith $"Invalid word id '%s{wordIdString}'"

        let upos = parseUpos (Array.tryItem 3 parts)
        let head =
            match defaultArg (Array.tryItem 6 parts) "0" with
            | "_" -> None
            | head -> Some(head |> byte)
        { ID = wordId
          Form = parts[1]
          Lemma = parseOptionalString parts[2]
          UniversalPartOfSpeech = upos;
          LanguageSpecificPartOfSpeech = parseString (Array.tryItem 4 parts);
          Features = parseDictionary (defaultArg (Array.tryItem 5 parts) "");
          Head = head;
          DependencyRelation = parseString (Array.tryItem 7 parts);
          Dependencies = parseString (Array.tryItem 8 parts);
          Miscellaneous = parseDictionary (defaultArg (Array.tryItem 9 parts) ""); }

    let private processWord (words:List<Word>) (comments: Dictionary<string, string>) p =
        match p with
        | "" -> ()
        | line when line.StartsWith('#') ->
            let comment = line.Substring(1)
            let p = comment.Split ([| '=' |], 2, StringSplitOptions.TrimEntries)
            if p.Length = 1 then
                comments.Add(comment, "")
            else
                comments.Add(p[0], p[1])
        | _ -> 
            let word = parseWord p
            words.Add(word)

    let private parseSentenceSeq (sentence: string seq) = 
        let words = List<Word>()
        let comments = Dictionary<string, string>()
        for p: string: string in sentence do
            processWord words comments p

        { Words = words; Comments = comments }

    let parseSentence (sentence: string) = 
        let parts = sentence.Split ([|'\n'|], StringSplitOptions.TrimEntries)
        parseSentenceSeq parts

    let parseBlock (block: string) =
        let blocks = block.ReplaceLineEndings("\r\n").Split "\r\n\r\n"
        blocks |> Seq.filter (fun x -> x <> "") |> Seq.map parseSentence |> Seq.toList

    let parseBlockSpan (block: ReadOnlySpan<char>) =
        let result = List<Sentence>()
        let mutable words = List<Word>()
        let mutable comments = Dictionary<string, string>()
        
        let moveNext () =
            if words.Count > 0 || comments.Count > 0 then
                result.Add({ Words = words; Comments = comments });
                words <- List<Word>()
                comments <- Dictionary<string, string>()

        for output in block.EnumerateLines() do
            if output.Length <> 0 then
                processWord words comments (output.ToString())
            else
                moveNext ()
        moveNext ()
        result

    let parseFile filename =
        let text = System.IO.File.ReadAllText filename
        parseBlock text

    let printWord word =
        let wordId = match word.ID with
                        | Range (start,finish) -> sprintf "%d-%d" start finish
                        | Position pos -> pos |> string
                        | NullPosition (start,finish) -> sprintf "%d.%d" start finish

        let upos = 
            match word.UniversalPartOfSpeech with
            | Some upos ->
                match upos with
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
            | None -> ""

        let head =
            match word.Head with
            | Some head -> head |> string
            | None -> "_"
        printf "%-7s" wordId
        printf "%-10s" word.Form
        printf "%-10s" (defaultArg word.Lemma "_")
        printf "%-10s" upos
        printf "%-20s" (defaultArg word.LanguageSpecificPartOfSpeech "_")
        printf " "
        printf "%-50s" (printDictionary word.Features)
        printf " "
        printf "%-6s" head
        printf "%-6s" (defaultArg word.DependencyRelation "_")
        printf "%-20s" (defaultArg word.Dependencies "_")
        printf " "
        printf "%s" (printDictionary word.Miscellaneous)
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