module Number2WorsTests

open NlpTools.Number2Words
open Xunit
open System.IO
open System.Linq

let values : obj[] seq = 
    let parseLine (line: string) =
        let parts = line.Split [| '|' |]
        let num = parts[0]|> int
        let gender = 
            match parts[1] with
            | "masculine" -> Masculine
            | "feminine" -> Feminine
            | _ -> failwith $"Unknown gender {parts[1]}"
        let form = parts[2]
        let case = parts[3]
        let expected = parts[4]
        [| num :> obj ; form; gender; case; expected |]
    let lines =
        File.ReadAllLines("uk.csv").Skip(1)
        |> Seq.map parseLine
    lines

[<Theory>]
[<MemberData("values")>]
let ``Ukrainian`` (number: int, form: string, gender: Gender, case: string, expected: string) =
    Assert.Equal(expected, (toText "uk" number form gender case))