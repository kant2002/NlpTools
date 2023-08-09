module Tests

open System
open Xunit
open NlpTools.CoNLLU

let replace (lineending: string) (str:string) =
    str.ReplaceLineEndings(lineending)

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
    Assert.Equal(Some(Adverb), parsed.Words[0].UniversalPartOfSpeech)
    Assert.Equal(Some("AB"), parsed.Words[0].LanguageSpecificPartOfSpeech)
    Assert.Empty(parsed.Words[0].Features)
    Assert.Equal(Position 2, parsed.Words[1].ID)
    Assert.Equal("var", parsed.Words[1].Form)
    Assert.Equal(Some("vara"), parsed.Words[1].Lemma)
    Assert.Equal(Some(Verb), parsed.Words[1].UniversalPartOfSpeech)
    Assert.Equal(Some("VB.PRET.ACT"), parsed.Words[1].LanguageSpecificPartOfSpeech)
    //Assert.NonEmpty(parsed.Words[1].Features)
    Assert.Equal(2, parsed.Words[1].Features.Count)
    Assert.Equal("Past", parsed.Words[1].Features["Tense"])
    Assert.Equal("Act", parsed.Words[1].Features["Voice"])

[<Fact>]
let ``Syntactic Annotation`` () =
    let sample = """
1    They     they    PRON    PRP    Case=Nom|Number=Plur               2    nsubj    2:nsubj|4:nsubj
2    buy      buy     VERB    VBP    Number=Plur|Person=3|Tense=Pres    0    root     0:root
3    and      and     CCONJ   CC     _                                  4    cc       4:cc
4    sell     sell    VERB    VBP    Number=Plur|Person=3|Tense=Pres    2    conj     0:root|2:conj
5    books    book    NOUN    NNS    Number=Plur                        2    obj      2:obj|4:obj
6    .        .       PUNCT   .      _                                  2    punct    2:punct
    """

    let parsed = parseSentence sample
    Assert.Empty(parsed.Comments)
    Assert.Equal(6, parsed.Words.Length)
    Assert.Equal(Position 1, parsed.Words[0].ID)
    Assert.Equal("They", parsed.Words[0].Form)
    Assert.Equal(Some("they"), parsed.Words[0].Lemma)
    Assert.Equal(Some(Pronoun), parsed.Words[0].UniversalPartOfSpeech)
    Assert.Equal(Some("PRP"), parsed.Words[0].LanguageSpecificPartOfSpeech)
    Assert.Equal(2, parsed.Words[0].Features.Count)
    Assert.Equal("Nom", parsed.Words[0].Features["Case"])
    Assert.Equal("Plur", parsed.Words[0].Features["Number"])
    Assert.Equal(Some(2uy), parsed.Words[0].Head)
    Assert.Equal(Some("nsubj"), parsed.Words[0].DependencyRelation)
    Assert.Equal(Some("2:nsubj|4:nsubj"), parsed.Words[0].Dependencies)

[<Fact>]
let ``Numbers in form can have whitespaces`` () =
    let sample = """
# sent_id = 2m0p
# text = У 1995-му Сінгапур із 26 000 дол. США на душу населення обійшов Британію (19 700).
# translit = U 1995-mu Sinhapur iz 26 000 dol. SŠA na dušu naselenńа obijšov Brytaniju (19 700).
1	У	у	ADP	Spsl	Case=Loc	2	case	2:case	Id=2m0q|LTranslit=u|Translit=U
2	1995-му	1995-ий	NOUN	Ao-mslf	Animacy=Inan|Case=Loc|Gender=Masc|Number=Sing	12	obl	12:obl	Id=2m0r|LTranslit=1995-yj|Translit=1995-mu
3	Сінгапур	Сінгапур	PROPN	Npmsnn	Animacy=Inan|Case=Nom|Gender=Masc|Number=Sing	12	nsubj	12:nsubj	Id=2m0s|LTranslit=Sinhapur|Translit=Sinhapur
4	із	із	ADP	Spsi	Case=Ins	6	case	6:case	Id=2m0t|LTranslit=iz|Translit=iz
5	26 000	26000	NUM	Mlc-i	Case=Ins|NumType=Card|Uninflect=Yes	6	nummod	6:nummod	Id=2m0v|LTranslit=26000|Translit=26 000
6	дол	дол.	NOUN	Y	Abbr=Yes|Animacy=Inan|Case=Ins|Gender=Masc|Number=Plur|Uninflect=Yes	3	nmod	3:nmod	Id=2m0w|LTranslit=dol.|SpaceAfter=No|Translit=dol
7	.	.	PUNCT	U	_	6	punct	6:punct	Id=2m0x|LTranslit=.|Translit=.
8	США	США	PROPN	Np-pgn	Animacy=Inan|Case=Gen|Number=Ptan|Uninflect=Yes	6	nmod	6:nmod	Id=2m0y|LTranslit=SŠA|Translit=SŠA
9	на	на	ADP	Spsa	Case=Acc	10	case	10:case	Id=2m0z|LTranslit=na|Translit=na
10	душу	душа	NOUN	Ncfsan	Animacy=Inan|Case=Acc|Gender=Fem|Number=Sing	6	nmod	6:nmod	Id=2m10|LTranslit=duša|Translit=dušu
11	населення	населення	NOUN	Ncnsgn	Animacy=Inan|Case=Gen|Gender=Neut|Number=Sing	10	nmod	10:nmod	Id=2m11|LTranslit=naselenńа|Translit=naselenńа
12	обійшов	обійти	VERB	Vmeis-sm	Aspect=Perf|Gender=Masc|Mood=Ind|Number=Sing|Tense=Past|VerbForm=Fin	0	root	0:root	Id=2m12|LTranslit=obijty|Translit=obijšov
13	Британію	Британія	PROPN	Npfsan	Animacy=Inan|Case=Acc|Gender=Fem|Number=Sing	12	obj	12:obj	Id=2m13|LTranslit=Brytanija|Translit=Brytaniju
14	(	(	PUNCT	U	_	15	punct	15:punct	Id=2m14|LTranslit=(|SpaceAfter=No|Translit=(
15	19 700	19700	NUM	Mlc-n	Case=Nom|NumType=Card|Uninflect=Yes	13	parataxis	13:parataxis	Id=2m16|LTranslit=19700|SpaceAfter=No|Translit=19 700
16	)	)	PUNCT	U	_	15	punct	15:punct	Id=2m17|LTranslit=)|SpaceAfter=No|Translit=)
17	.	.	PUNCT	U	_	12	punct	12:punct	Id=2m18|LTranslit=.|Translit=.
    """

    let parsed = parseSentence sample
    Assert.Equal(3, parsed.Comments.Count)
    Assert.Equal(17, parsed.Words.Length)

[<Fact>]
let ``UPOS can be missing`` () =
    let sample = """
    1-2   He's      _         _       _       _                                 _   _       _   _
    """

    let parsed = parseSentence sample
    Assert.Empty(parsed.Comments)
    Assert.Equal(1, parsed.Words.Length)
    Assert.Equal("He's", parsed.Words[0].Form)
    Assert.Equal(None, parsed.Words[0].Lemma)
    Assert.Equal(None, parsed.Words[0].UniversalPartOfSpeech)
    Assert.Equal(None, parsed.Words[0].LanguageSpecificPartOfSpeech)
    Assert.Empty(parsed.Words[0].Features)
    Assert.Equal(None, parsed.Words[0].Head)
    Assert.Equal(None, parsed.Words[0].DependencyRelation)
    Assert.Equal(None, parsed.Words[0].Dependencies)

[<Fact>]
let ``Parse miscellaneous`` () =
    let sample = """
    1   Slovenská   slovenský   ADJ     AAFS1----1A---- Case=Nom|Degree=Pos|Gender=Fem|Number=Sing|Polarity=Pos 2 amod _ _
    2   ústava      ústava      NOUN    NNFS1-----A---- Case=Nom|Gender=Fem|Number=Sing|Polarity=Pos 0 root _ SpaceAfter=No
    3   :           :           PUNCT   Z:------------- _          2       punct   _       _
    4   pro         pro         ADP     RR--4---------- Case=Acc   2       appos   _       LId=pro-1
    5   i           i           CCONJ   J^------------- _          6       cc      _       LId=i-1
    6   proti       proti       ADP     RR--3---------- Case=Dat   4       conj    _       LId=proti-1
    """

    let parsed = parseSentence sample
    Assert.Empty(parsed.Comments)
    Assert.Equal(6, parsed.Words.Length)
    Assert.Equal(1, parsed.Words[1].Miscellaneous.Count)
    Assert.Equal("No", parsed.Words[1].Miscellaneous["SpaceAfter"])

[<Fact>]
let ``Parse comments`` () =
    let sample = """
    # newdoc id = mf920901-001
    # newpar id = mf920901-001-p1
    # sent_id = mf920901-001-p1s1A
    # text = Slovenská ústava: pro i proti
    # text_en = Slovak constitution: pros and cons
    1   Slovenská   slovenský   ADJ     AAFS1----1A---- Case=Nom|Degree=Pos|Gender=Fem|Number=Sing|Polarity=Pos 2 amod _ _
    2   ústava      ústava      NOUN    NNFS1-----A---- Case=Nom|Gender=Fem|Number=Sing|Polarity=Pos 0 root _ SpaceAfter=No
    3   :           :           PUNCT   Z:------------- _          2       punct   _       _
    4   pro         pro         ADP     RR--4---------- Case=Acc   2       appos   _       LId=pro-1
    5   i           i           CCONJ   J^------------- _          6       cc      _       LId=i-1
    6   proti       proti       ADP     RR--3---------- Case=Dat   4       conj    _       LId=proti-1
    """

    let parsed = parseSentence sample
    Assert.Equal(5, parsed.Comments.Count)
    Assert.Equal("mf920901-001", parsed.Comments["newdoc id"])
    Assert.Equal("mf920901-001-p1", parsed.Comments["newpar id"])
    Assert.Equal("mf920901-001-p1s1A", parsed.Comments["sent_id"])
    Assert.Equal("Slovenská ústava: pro i proti", parsed.Comments["text"])
    Assert.Equal("Slovak constitution: pros and cons", parsed.Comments["text_en"])

[<Fact>]
let ``Parse file`` () =
    let sample = """
# author = Багряний Іван
# doc_title = Сад Гетсиманський
# newdoc id = 028g
# newpar id = 02tb
# sent_id = 02to
# text = Дідусь, той що атестував, посміхнувся й спитав:
# translit = Diduś, toj ščo atestuvav, posmichnuvśа j spytav:
1	Дідусь	дідусь	NOUN	Ncmsny	Animacy=Anim|Case=Nom|Gender=Masc|Number=Sing	7	nsubj	7:nsubj|9:nsubj	Id=02tp|LTranslit=diduś|SpaceAfter=No|Translit=Diduś
2	,	,	PUNCT	U	_	3	punct	3:punct	Id=02tq|LTranslit=,|Translit=,
3	той	той	DET	Pd--m-sna	Case=Nom|Gender=Masc|Number=Sing|PronType=Dem	7	dislocated	5:nsubj|7:dislocated|9:dislocated	Id=02tr|LTranslit=toj|Translit=toj
4	що	що	SCONJ	Css	_	5	mark	5:mark	Id=02ts|LTranslit=ščo|Translit=ščo
5	атестував	атестувати	VERB	Vmpis-sm	Aspect=Imp|Gender=Masc|Mood=Ind|Number=Sing|Tense=Past|VerbForm=Fin	3	acl:relcl	3:acl:relcl	Id=02tt|LTranslit=atestuvaty|SpaceAfter=No|Translit=atestuvav
6	,	,	PUNCT	U	_	5	punct	5:punct	Id=02tu|LTranslit=,|Translit=,
7	посміхнувся	посміхнутися	VERB	Vmeis-sm	Aspect=Perf|Gender=Masc|Mood=Ind|Number=Sing|Tense=Past|VerbForm=Fin	0	root	0:root	Id=02tv|LTranslit=posmichnutyśа|Translit=posmichnuvśа
8	й	й	CCONJ	Ccs	_	9	cc	9:cc	Id=02tw|LTranslit=j|Translit=j
9	спитав	спитати	VERB	Vmeis-sm	Aspect=Perf|Gender=Masc|Mood=Ind|Number=Sing|Tense=Past|VerbForm=Fin	7	conj	0:root|7:conj	Id=02tx|LTranslit=spytaty|SpaceAfter=No|Translit=spytav
10	:	:	PUNCT	U	_	7	punct	7:punct	Id=02ty|LTranslit=:|Translit=:

# newpar id = 02tz
# sent_id = 2emh
# text = — Ви перед цим сиділи в тюрмі?
# translit = — Vy pered cym sydily v ťurmi?
1	—	—	PUNCT	U	PunctType=Dash	5	punct	5:punct	Id=02u0|LTranslit=—|Translit=—
2	Ви	ви	PRON	Pp-2-ypnn	Animacy=Anim|Case=Nom|Number=Plur|Person=2|PronType=Prs	5	nsubj	5:nsubj	Id=02u1|LTranslit=vy|Translit=Vy
3	перед	перед	ADP	Spsi	Case=Ins	4	case	4:case	Id=02u2|LTranslit=pered|Translit=pered
4	цим	це	PRON	Pd--nnsin	Animacy=Inan|Case=Ins|Gender=Neut|Number=Sing|PronType=Dem	5	obl	5:obl	Id=02u3|LTranslit=ce|Translit=cym
5	сиділи	сидіти	VERB	Vmpis-p	Aspect=Imp|Mood=Ind|Number=Plur|Tense=Past|VerbForm=Fin	0	root	0:root	Id=02u4|LTranslit=sydity|Translit=sydily
6	в	в	ADP	Spsl	Case=Loc	7	case	7:case	Id=02u5|LTranslit=v|Translit=v
7	тюрмі	тюрма	NOUN	Ncfsln	Animacy=Inan|Case=Loc|Gender=Fem|Number=Sing	5	obl	5:obl	Id=02u6|LTranslit=ťurma|SpaceAfter=No|Translit=ťurmi
8	?	?	PUNCT	U	_	5	punct	5:punct	Id=02u7|LTranslit=?|Translit=?

"""

    let parsed = parseBlock (sample |> replace "\r\n")
    Assert.Equal(2, parsed.Length)

    let parsed = parseBlock (sample |> replace "\n")
    Assert.Equal(2, parsed.Length)

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
