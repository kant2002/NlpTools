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
