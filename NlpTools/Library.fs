namespace NlpTools

open System.Collections.Generic
open System
open System.Linq
open System.Text
open System.Runtime.CompilerServices
open System.Globalization

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

    [<IsByRefLike; Struct>]
    type SpanSeparatorEnumerator =
        struct
            val mutable private _remaining: ReadOnlySpan<char>;
            val mutable private _isEnumeratorActive: bool
            val mutable private _current: ReadOnlySpan<char>;
            val Separators: ReadOnlySpan<char>;

            new(buffer: ReadOnlySpan<char>, separators: ReadOnlySpan<char>) = { _remaining = buffer ; _isEnumeratorActive = true; _current = ReadOnlySpan<char>(); Separators = separators }

            ///<summary>Returns this instance as an enumerator.</summary>
            ///<returns>This instance as an enumerator.</returns>
            member self.GetEnumerator() = self
            

            ///<summary>Advances the enumerator to the next line of the span.</summary>
            ///<returns><see langword="true" /> if the enumerator successfully advanced to the next line; <see langword="false" /> if the enumerator has advanced past the end of the span.</returns>
            member self.MoveNext() =
                if not (self._isEnumeratorActive) then
                    false // EOF previously reached or enumerator was never initialized
                else
                    let remaining = self._remaining;

                    let idx = remaining.IndexOfAny(self.Separators);

                    if ((uint)idx < (uint)remaining.Length) then

                        let stride = 1

                        self._current <- remaining.Slice(0, idx)
                        self._remaining <- remaining.Slice(idx + stride)

                    else
                        // We've reached EOF, but we still need to return 'true' for this final
                        // iteration so that the caller can query the Current property once more.

                        self._current <- remaining
                        self._remaining <- ReadOnlySpan<char>()
                        self._isEnumeratorActive <- false

                    true;

            ///<summary>Gets the line at the current position of the enumerator.</summary>
            ///<returns>The line at the current position of the enumerator.</returns>
            member self.Current = self._current
        end

    let private parseDictionary (features: ReadOnlySpan<char>) =
        let result = Dictionary<string, string>()
        if features.Length > 0 then
            let separatorEnumerator = SpanSeparatorEnumerator(features, "|")
            for featureDef in separatorEnumerator do
                if not(featureDef.Length = 1 && featureDef[0] = '_') then
                    let eqPos = featureDef.IndexOf('=')
                    result.Add(featureDef.Slice(0, eqPos).ToString(), featureDef.Slice(eqPos + 1).ToString())

        result

    let private printDictionary (features: Dictionary<string, string>) =
        let x = features.Select(fun x -> $"{x.Key}={x.Value}")
        String.concat "|" x

    let private parseOptionalString value =
        match value with
        | "_" -> None
        | _ -> Some(value)

    let private parseUposToken (pos :ReadOnlySpan<char>) =
        if pos.SequenceEqual("ADJ") then
            Some(Adjective)
        elif pos.SequenceEqual("ADP") then
            Some(Adposition)
        elif pos.SequenceEqual("ADV") then
            Some(Adverb)
        elif pos.SequenceEqual("AUX") then
            Some(Auxiliary)
        elif pos.SequenceEqual("CCONJ") then
            Some(CoordinatingConjunction)
        elif pos.SequenceEqual("DET") then
            Some(Determiner)
        elif pos.SequenceEqual("INTJ") then
            Some(Interjection)
        elif pos.SequenceEqual("NOUN") then
            Some(Noun)
        elif pos.SequenceEqual("NUM") then
            Some(Numeral)
        elif pos.SequenceEqual("PART") then
            Some(Particle)
        elif pos.SequenceEqual("PRON") then
            Some(Pronoun)
        elif pos.SequenceEqual("PROPN") then
            Some(ProperNoun)
        elif pos.SequenceEqual("PUNCT") then
            Some(Punctuation)
        elif pos.SequenceEqual("SCONJ") then
            Some(SubordinatingConjunction)
        elif pos.SequenceEqual("SYM") then
            Some(Symbol)
        elif pos.SequenceEqual("VERB") then
            Some(Verb)
        elif pos.SequenceEqual("X") then
            Some(Other)
        elif pos.SequenceEqual("_") then
            None
        else
            failwith $"Unknown universal part of speech %s{pos.ToString()}"
    
    let private parseToken (v: ReadOnlySpan<char>) =
        if v.Length = 0 then
            None
        elif v.Length = 1 && v[0] = '_' then 
            None
        else
            Some(v.ToString())

    [<Struct; IsByRefLike>]
    type ParseResult =
        struct
            val Token: ReadOnlySpan<char>;
            val Leftover: ReadOnlySpan<char>;
            new (token: ReadOnlySpan<char>, leftover: ReadOnlySpan<char>) = { Token = token; Leftover = leftover }
        end

    let nextToken (word: ReadOnlySpan<char>, parseResult : byref<ParseResult>) =
        let tabToken = word.IndexOf '\t'
        if tabToken = -1 then
            parseResult <- ParseResult(word, ReadOnlySpan<char>())
        else
            parseResult <- ParseResult(word.Slice(0,tabToken), word.Slice(tabToken + 1))

    let private parseWord (word: ReadOnlySpan<char>) =
        let mutable parseResult : ParseResult = ParseResult()
        nextToken (word, &parseResult)

        let wordIdSpan = parseResult.Token
        let wordId = match wordIdSpan.IndexOf '-' with
                        | -1 -> match wordIdSpan.IndexOf '.' with
                                | -1 -> if wordIdSpan.Length > 0 then
                                            let head = Int32.Parse (wordIdSpan, CultureInfo.InvariantCulture)
                                            Position (head)
                                        else
                                            failwith $"Invalid word id '%s{wordIdSpan.ToString()}'"
                                | dotPos ->
                                    let head = Int32.Parse (wordIdSpan.Slice(0, dotPos), CultureInfo.InvariantCulture)
                                    let tail = Int32.Parse (wordIdSpan.Slice(dotPos + 1), CultureInfo.InvariantCulture)
                                    NullPosition (head, tail)
                        | dashPos -> 
                            let head = Int32.Parse (wordIdSpan.Slice(0, dashPos), CultureInfo.InvariantCulture)
                            let tail = Int32.Parse (wordIdSpan.Slice(dashPos + 1), CultureInfo.InvariantCulture)
                            Range (head, tail)
                            
        nextToken (parseResult.Leftover, &parseResult)
        let form = parseResult.Token.ToString()

        nextToken (parseResult.Leftover, &parseResult)
        let lemma = parseResult.Token.ToString()

        nextToken (parseResult.Leftover, &parseResult)
        let upos = if parseResult.Token.Length = 0 then None else parseUposToken parseResult.Token

        nextToken (parseResult.Leftover, &parseResult)
        let lpos = parseToken parseResult.Token
        nextToken (parseResult.Leftover, &parseResult)
        let features = parseDictionary parseResult.Token
        nextToken (parseResult.Leftover, &parseResult)
        let head =
            if parseResult.Token.Length = 0 then 
                Some(0uy)
            elif parseResult.Token.Length = 1 && parseResult.Token[0] = '_' then
                None
            else
                Some(Byte.Parse (parseResult.Token, CultureInfo.InvariantCulture))

        nextToken (parseResult.Leftover, &parseResult)
        let deprel = parseToken parseResult.Token
        nextToken (parseResult.Leftover, &parseResult)
        let deps = parseToken parseResult.Token
        nextToken (parseResult.Leftover, &parseResult)
        let misc = parseDictionary parseResult.Token

        { ID = wordId
          Form = form
          Lemma = parseOptionalString lemma
          UniversalPartOfSpeech = upos;
          LanguageSpecificPartOfSpeech = lpos;
          Features = features;
          Head = head;
          DependencyRelation = deprel;
          Dependencies = deps;
          Miscellaneous = misc; }

    let private processWord (words:List<Word>) (comments: Dictionary<string, string>) (p: ReadOnlySpan<char>) =
        match p with
        | line when line.StartsWith("#") ->
            let comment = line.Slice(1)
            let equalIndex = comment.IndexOf('=')
            if equalIndex = -1 then
                comments.Add(comment.Trim().ToString(), "")
            else
                comments.Add(comment.Slice(0, equalIndex).Trim().ToString(), comment.Slice(equalIndex + 1).Trim().ToString())
        | _ -> 
            let word = parseWord p
            words.Add(word)

    let private parseSentenceSeq (sentence: SpanLineEnumerator) = 
        let words = List<Word>()
        let comments = Dictionary<string, string>()
        for p in sentence do
            let word = p.Trim()
            if word.Length <> 0 then
                processWord words comments word

        { Words = words; Comments = comments }

    let parseSentenceSpan (sentence: ReadOnlySpan<char>) = 
        parseSentenceSeq (sentence.EnumerateLines ())

    let parseSentence (sentence: string) = 
        parseSentenceSpan (sentence.AsSpan())

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
                processWord words comments output
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