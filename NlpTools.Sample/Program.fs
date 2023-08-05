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

printfn "%s" sample
