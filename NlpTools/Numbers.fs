namespace NlpTools

open Uk
open Kk

module Number2Words =

    let toText lang (number: int64) form gender case =
        match lang with
        | "uk" -> toTextUk number form gender case
        | "kk" -> toTextKk number form gender case
        | _ -> failwith "Only Ukrainian and Kazakh are supported"

    //let toText lang (number: int32) form gender case =
    //    toText lang (number |> int64) form gender case