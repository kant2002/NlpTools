module Tests

open System
open Xunit
open NlpTools.CoNLLU

[<Fact>]
let ``Minimal information`` () =
    let sample = """
    1-2    vámonos   _
    1      vamos     ir
    2      nos       nosotros
    3-4    al        _
    3      a         a
    4      el        el
    5      mar       mar
    """

    let parsed = parseSentence sample
    Assert.Empty(parsed.Comments)
    Assert.Equal(7, parsed.Words.Length)
    Assert.Equal(Range (1,2), parsed.Words[0].ID)
    Assert.Equal(None, parsed.Words[0].Lemma)
    Assert.Equal(Position 1, parsed.Words[1].ID)
    Assert.Equal("vamos", parsed.Words[1].Form)
    Assert.Equal(Some("ir"), parsed.Words[1].Lemma)
    Assert.Equal(Position 2, parsed.Words[2].ID)

[<Fact>]
let ``Null positions parsed`` () =
    let sample = """
    1      Sue       Sue
    2      likes     like
    3      coffee    coffee
    4      and       and
    5      Bill      Bill
    5.1    likes     like
    6      tea       tea
    """

    let parsed = parseSentence sample
    Assert.Empty(parsed.Comments)
    Assert.Equal(7, parsed.Words.Length)
    Assert.Equal(Position 1, parsed.Words[0].ID)
    Assert.Equal("Sue", parsed.Words[0].Form)
    Assert.Equal(Some("Sue"), parsed.Words[0].Lemma)
    Assert.Equal(Position 2, parsed.Words[1].ID)
    Assert.Equal("likes", parsed.Words[1].Form)
    Assert.Equal(Some("like"), parsed.Words[1].Lemma)
    Assert.Equal(NullPosition (5,1), parsed.Words[5].ID)

[<Fact>]
let ``Morphological Annotation`` () =
    let sample = """
1    Då      då     ADV      AB                    _
2    var     vara   VERB     VB.PRET.ACT           Tense=Past|Voice=Act
3    han     han    PRON     PN.UTR.SIN.DEF.NOM    Case=Nom|Definite=Def|Gender=Com|Number=Sing
4    elva    elva   NUM      RG.NOM                Case=Nom|NumType=Card
5    år      år     NOUN     NN.NEU.PLU.IND.NOM    Case=Nom|Definite=Ind|Gender=Neut|Number=Plur
6    .       .      PUNCT    DL.MAD                _
    """

    let parsed = parseSentence sample
    Assert.Empty(parsed.Comments)
    Assert.Equal(6, parsed.Words.Length)
    Assert.Equal(Position 1, parsed.Words[0].ID)
    Assert.Equal("Då", parsed.Words[0].Form)
    Assert.Equal(Some("då"), parsed.Words[0].Lemma)
    Assert.Equal(Adverb, parsed.Words[0].UniversalPartOfSpeech)
    Assert.Equal("AB", parsed.Words[0].LanguageSpecificPartOfSpeech)
    Assert.Empty(parsed.Words[0].Features)
    Assert.Equal(Position 2, parsed.Words[1].ID)
    Assert.Equal("var", parsed.Words[1].Form)
    Assert.Equal(Some("vara"), parsed.Words[1].Lemma)
    Assert.Equal(Verb, parsed.Words[1].UniversalPartOfSpeech)
    Assert.Equal("VB.PRET.ACT", parsed.Words[1].LanguageSpecificPartOfSpeech)
    //Assert.NonEmpty(parsed.Words[1].Features)
    Assert.Equal(2, parsed.Words[1].Features.Count)
    Assert.Equal("Past", parsed.Words[1].Features["Tense"])
    Assert.Equal("Act", parsed.Words[1].Features["Voice"])

[<Fact>]
let ``Reconstruct sentence`` () =
    let sample = """
    1-2    vámonos   _
    1      vamos     ir
    2      nos       nosotros
    3-4    al        _
    3      a         a
    4      el        el
    5      mar       mar
    """

    let parsed = parseSentence sample
    let reconstructed = reconstructSentence parsed
    Assert.Equal("vámonos al mar", reconstructed)

[<Fact>]
let ``Null positions reconstructed`` () =
    let sample = """
    1      Sue       Sue
    2      likes     like
    3      coffee    coffee
    4      and       and
    5      Bill      Bill
    5.1    likes     like
    6      tea       tea
    """

    let parsed = parseSentence sample
    let reconstructed = reconstructSentence parsed
    Assert.Equal("Sue likes coffee and Bill tea", reconstructed)
