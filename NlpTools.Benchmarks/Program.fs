open BenchmarkDotNet.Attributes
open BenchmarkDotNet.Running
open NlpTools.CoNLLU
open System.IO

let sample1 = """
# sent_id = 02wd
# text = Сам Андрій нікого ні про що не розпитував, нічим абсолютно не цікавився, ні до кого особливо (на сповид) не приглядався, а так само не хвилювався,— був індиферентний і зовсім спокійний.
# translit = Sam Andrij nikoho ni pro ščo ne rozpytuvav, ničym absoľutno ne cikavyvśа, ni do koho osoblyvo (na spovyd) ne pryhľаdavśа, a tak samo ne chvyľuvavśа,— buv indyferentnyj i zovsim spokijnyj.
1	Сам	сам	DET	Px--m-sna	Case=Nom|Gender=Masc|Number=Sing|PronType=Prs|Reflex=Yes	2	det	2:det	Id=02we|LTranslit=sam|Translit=Sam
2	Андрій	Андрій	PROPN	Npmsny	Animacy=Anim|Case=Nom|Gender=Masc|NameType=Giv|Number=Sing	8	nsubj	8:nsubj|13:nsubj|24:nsubj|30:nsubj|34:nsubj|37:nsubj	Id=02wf|LTranslit=Andrij|Translit=Andrij
3	нікого	ніхто	PRON	Pz---y-an	Animacy=Anim|Case=Acc|PronType=Neg	8	obj	8:obj	Id=02wg|LTranslit=nichto|Translit=nikoho
4	ні	ні	PART	Q	Polarity=Neg	6	advmod	6:advmod	Id=02wh|LTranslit=ni|Translit=ni
5	про	про	ADP	Spsa	Case=Acc	6	case	6:case	Id=02wi|LTranslit=pro|Translit=pro
6	що	що	PRON	Pr--nnsan	Animacy=Inan|Case=Acc|Gender=Neut|Number=Sing|PronType=Rel	8	obl	8:obl	Id=02wj|LTranslit=ščo|Translit=ščo
7	не	не	PART	Q	Polarity=Neg	8	advmod	8:advmod	Id=02wk|LTranslit=ne|Translit=ne
8	розпитував	розпитувати	VERB	Vmpis-sm	Aspect=Imp|Gender=Masc|Mood=Ind|Number=Sing|Tense=Past|VerbForm=Fin	0	root	0:root	Id=02wl|LTranslit=rozpytuvaty|SpaceAfter=No|Translit=rozpytuvav
9	,	,	PUNCT	U	_	13	punct	13:punct	Id=02wm|LTranslit=,|Translit=,
10	нічим	ніщо	PRON	Pz---n-in	Animacy=Inan|Case=Ins|PronType=Neg	13	obl	13:obl	Id=02wn|LTranslit=niščo|Translit=ničym
11	абсолютно	абсолютно	ADV	R	_	13	advmod	13:advmod	Id=02wo|LTranslit=absoľutno|Translit=absoľutno
12	не	не	PART	Q	Polarity=Neg	13	advmod	13:advmod	Id=02wp|LTranslit=ne|Translit=ne
13	цікавився	цікавитися	VERB	Vmpis-sm	Aspect=Imp|Gender=Masc|Mood=Ind|Number=Sing|Tense=Past|VerbForm=Fin	8	conj	0:root|8:conj	Id=02wq|LTranslit=cikavytyśа|SpaceAfter=No|Translit=cikavyvśа
14	,	,	PUNCT	U	_	24	punct	24:punct	Id=02wr|LTranslit=,|Translit=,
15	ні	ні	PART	Q	Polarity=Neg	17	advmod	17:advmod	Id=02ws|LTranslit=ni|Translit=ni
16	до	до	ADP	Spsg	Case=Gen	17	case	17:case	Id=02wt|LTranslit=do|Translit=do
17	кого	хто	PRON	Pr--mysgn	Animacy=Anim|Case=Gen|Gender=Masc|Number=Sing|PronType=Rel	24	obl	24:obl	Id=02wu|LTranslit=chto|Translit=koho
18	особливо	особливо	ADV	R	_	24	advmod	24:advmod	Id=02wv|LTranslit=osoblyvo|Translit=osoblyvo
19	(	(	PUNCT	U	_	21	punct	21:punct	Id=02ww|LTranslit=(|SpaceAfter=No|Translit=(
20	на	на	ADP	Spsa	Case=Acc	21	case	21:case	Id=02wx|LTranslit=na|Translit=na
21	сповид	сповид	NOUN	Ncmsan	Animacy=Inan|Case=Acc|Gender=Masc|Number=Sing	18	parataxis:discourse	18:parataxis:discourse	Id=02wy|LTranslit=spovyd|SpaceAfter=No|Translit=spovyd
22	)	)	PUNCT	U	_	21	punct	21:punct	Id=02wz|LTranslit=)|Translit=)
23	не	не	PART	Q	Polarity=Neg	24	advmod	24:advmod	Id=02x0|LTranslit=ne|Translit=ne
24	приглядався	приглядатися	VERB	Vmpis-sm	Aspect=Imp|Gender=Masc|Mood=Ind|Number=Sing|Tense=Past|VerbForm=Fin	8	conj	0:root|8:conj	Id=02x1|LTranslit=pryhľаdatyśа|SpaceAfter=No|Translit=pryhľаdavśа
25	,	,	PUNCT	U	_	30	punct	30:punct	Id=02x2|LTranslit=,|Translit=,
26	а	а	CCONJ	Ccs	_	30	cc	30:cc	Id=02x3|LTranslit=a|Translit=a
27	так	так	ADV	Pd------r	PronType=Dem	30	advmod	30:advmod	Id=02x4|LTranslit=tak|Translit=tak
28	само	само	ADV	R	_	27	advmod	27:advmod	Id=02x5|LTranslit=samo|Translit=samo
29	не	не	PART	Q	Polarity=Neg	30	advmod	30:advmod	Id=02x6|LTranslit=ne|Translit=ne
30	хвилювався	хвилюватися	VERB	Vmpis-sm	Aspect=Imp|Gender=Masc|Mood=Ind|Number=Sing|Tense=Past|VerbForm=Fin	8	conj	0:root|8:conj	Id=02x7|LTranslit=chvyľuvatyśа|SpaceAfter=No|Translit=chvyľuvavśа
31	,	,	PUNCT	U	_	34	punct	34:punct	Id=02x8|LTranslit=,|SpaceAfter=No|Translit=,
32	—	—	PUNCT	U	PunctType=Dash	34	punct	34:punct	Id=02x9|LTranslit=—|Translit=—
33	був	бути	AUX	Vapis-sm	Aspect=Imp|Gender=Masc|Mood=Ind|Number=Sing|Tense=Past|VerbForm=Fin	34	cop	34:cop|37:cop	Id=02xa|LTranslit=buty|Translit=buv
34	індиферентний	індиферентний	ADJ	Ao-msnf	Case=Nom|Gender=Masc|Number=Sing	8	conj	0:root|8:conj	Id=02xb|LTranslit=indyferentnyj|Translit=indyferentnyj
35	і	і	CCONJ	Ccs	_	37	cc	37:cc	Id=02xc|LTranslit=i|Translit=i
36	зовсім	зовсім	ADV	R	_	37	advmod	37:advmod	Id=02xd|LTranslit=zovsim|Translit=zovsim
37	спокійний	спокійний	ADJ	Afpmsnf	Case=Nom|Degree=Pos|Gender=Masc|Number=Sing	34	conj	0:root|34:conj	Id=02xe|LTranslit=spokijnyj|SpaceAfter=No|Translit=spokijnyj
38	.	.	PUNCT	U	_	8	punct	8:punct	Id=02xf|LTranslit=.|Translit=.
"""

let sample2 = File.ReadAllText "uk_iu-ud-dev.conllu"

[<MemoryDiagnoser>]
type SentenceBench() =
    [<Benchmark>]
    member _.Sentence38Words() = parseSentence sample1

    [<Benchmark>]
    member _.LotOfSentences() = parseBlock sample2

[<EntryPoint>]
let main argv =
    BenchmarkRunner.Run<SentenceBench>() |> ignore
    0 // return an integer exit code
