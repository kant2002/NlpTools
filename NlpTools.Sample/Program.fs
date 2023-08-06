open NlpTools.CoNLLU

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
printSentence parsed

let sample2 = """
1    Då      då     ADV      AB                    _
2    var     vara   VERB     VB.PRET.ACT           Tense=Past|Voice=Act
3    han     han    PRON     PN.UTR.SIN.DEF.NOM    Case=Nom|Definite=Def|Gender=Com|Number=Sing
4    elva    elva   NUM      RG.NOM                Case=Nom|NumType=Card
5    år      år     NOUN     NN.NEU.PLU.IND.NOM    Case=Nom|Definite=Ind|Gender=Neut|Number=Plur
6    .       .      PUNCT    DL.MAD                _
    """

let parsed2 = parseSentence sample2
printSentence parsed2

let sample3 = """
1    They     they    PRON    PRP    Case=Nom|Number=Plur               2    nsubj    2:nsubj|4:nsubj
2    buy      buy     VERB    VBP    Number=Plur|Person=3|Tense=Pres    0    root     0:root
3    and      and     CCONJ   CC     _                                  4    cc       4:cc
4    sell     sell    VERB    VBP    Number=Plur|Person=3|Tense=Pres    2    conj     0:root|2:conj
5    books    book    NOUN    NNS    Number=Plur                        2    obj      2:obj|4:obj
6    .        .       PUNCT   .      _                                  2    punct    2:punct
    """

let parsed3 = parseSentence sample3
printSentence parsed3
