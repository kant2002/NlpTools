﻿open NlpTools.CoNLLU
open System.IO

let sample = """
1-2	vámonos	_
1	vamos	ir
2	nos	nosotros
3-4	al	_
3	a	a
4	el	el
5	mar	mar
"""

let parsed = parseSentence sample
printSentence parsed
printfn ""

let sample2 = """
1	Då	då	ADV	AB	_
2	var	vara	VERB	VB.PRET.ACT	Tense=Past|Voice=Act
3	han	han	PRON	PN.UTR.SIN.DEF.NOM	Case=Nom|Definite=Def|Gender=Com|Number=Sing
4	elva	elva	NUM	RG.NOM	Case=Nom|NumType=Card
5	år	år	NOUN	NN.NEU.PLU.IND.NOM	Case=Nom|Definite=Ind|Gender=Neut|Number=Plur
6	.	.	PUNCT	DL.MAD	_
    """

let parsed2 = parseSentence sample2
printSentence parsed2
printfn ""

let sample3 = """
# newdoc id = mf920901-001
# newpar id = mf920901-001-p1
# sent_id = mf920901-001-p1s1A
# text = Slovenská ústava: pro i proti
# text_en = Slovak constitution: pros and cons
1	Slovenská	slovenský	ADJ	AAFS1----1A----	Case=Nom|Degree=Pos|Gender=Fem|Number=Sing|Polarity=Pos	2	amod	_	_
2	ústava	ústava	NOUN	NNFS1-----A----	Case=Nom|Gender=Fem|Number=Sing|Polarity=Pos	0	root	_	SpaceAfter=No
3	:	:	PUNCT	Z:-------------	_	2	punct	_	_
4	pro	pro	ADP	RR--4----------	Case=Acc	2	appos	_	LId=pro-1
5	i	i	CCONJ	J^-------------	_	6	cc	_	LId=i-1
6	proti	proti	ADP	RR--3----------	Case=Dat	4	conj	_	LId=proti-1
    """

let parsed3 = parseSentence sample3
printSentence parsed3
printfn ""

[<EntryPoint>]
let main(args) =
    if args.Length >= 1 then
        // "../../../uk_iu-ud-train.conllu.txt"
        // "../../../en_ewt-ud-train.conllu.txt"
        if File.Exists args[0] then
            let p = parseFile args[0]
            printfn "Parsed %d sentences" p.Length

            let translit = 
                p 
                    |> Seq.collect(fun x -> x.Words)
                    |> Seq.filter(fun x -> x.Miscellaneous.ContainsKey("Translit") && x.UniversalPartOfSpeech <> Some(Punctuation) && x.UniversalPartOfSpeech <> Some(Numeral))
                    |> Seq.map(fun x-> x.Miscellaneous["Translit"])
                    |> Seq.distinct

            let outputfile = defaultArg (Array.tryItem 1 args) "translit.txt"
            File.WriteAllLines (outputfile, translit)

    0
