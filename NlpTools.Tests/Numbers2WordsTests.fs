module Number2WorsTests

open NlpTools.Number2Words
open NlpTools.Types
open Xunit
open System.IO
open System.Linq

let parseLine (line: string) =
    let parts = line.Split [| '|' |]
    let num = parts[0]|> int64
    let gender = 
        match parts[1] with
        | "masculine" -> Masculine
        | "feminine" -> Feminine
        | _ -> failwith $"Unknown gender {parts[1]}"
    let form = parts[2]
    let case = parts[3]
    let expected = parts[4]
    [| num :> obj ; form; gender; case; expected |]
let fit_int64 (line: string)  =
    let parts = line.Split [| '|' |]
    parts[0].Length <= 21

let values_uk : obj[] seq = 
    let lines =
        File.ReadAllLines("uk.csv").Skip(1)
        |> Seq.filter fit_int64
        |> Seq.map parseLine
    lines

let values_kk : obj[] seq = 
    let lines =
        File.ReadAllLines("kk.csv").Skip(1)
        |> Seq.filter fit_int64
        |> Seq.map parseLine
    lines

[<Theory>]
[<MemberData("values_uk")>]
let ``Ukrainian`` (number: int64, form: string, gender: Gender, case: string, expected: string) =
    Assert.Equal(expected, (toText "uk" number form gender case))

[<Theory>]
[<MemberData("values_kk")>]
let ``Kazakh`` (number: int64, form: string, gender: Gender, case: string, expected: string) =
    Assert.Equal(expected, (toText "kk" number form gender case))