namespace NlpTools

module Kk =
    open System.Text
    open Types

    let private small = [|
        "бір";
        "екі";
        "үш";
        "төрт";
        "бес";
        "алты";
        "жеті";
        "сегіз";
        "тоғыз";
    |]

    let private  tens_text = [|
        "он"
        "жиырма"
        "отыз"
        "қырық"
        "елу"
        "алпыс"
        "жетпіс"
        "сексен"
        "тоқсан"
    |]

    let private large_numbers = [|
        "мың";
        "миллион";
        "миллиард";
        "триллион";
        "квадриллион";
        "квинтиллион";
        "секстиллион";
        "септиллион";
        "октиллион";
        "нониллион";
    |]

    let private printThousands (result: StringBuilder) number =
        let hundreds = (number / 100) % 10
        let tens = (number / 10) % 10
        let ones = number % 10
        if hundreds > 0 then
            if hundreds > 1 then
                result.Append(small[hundreds - 1]) |> ignore
                result.Append(" ") |> ignore
            result.Append("жүз") |> ignore
            if tens > 0 || ones > 0 then
                result.Append(" ") |> ignore
        if tens >= 1 then
            result.Append(tens_text[tens - 1]) |> ignore
            if ones > 0 then
                result.Append(" ") |> ignore

        if ones > 0 then
            result.Append(small[ones - 1]) |> ignore

    let toTextKk (number: int64) form gender case =
        if number = 0 
            then "нөл"
        else
            let result = StringBuilder()
            let rec x depth num =
                let reminder = (num % 1000L) |> int
                let basic = num / 1000L
                if (basic = 0) then
                    printThousands result reminder
                    if depth > 0 then
                        result.Append(" ") |> ignore
                        result.Append(large_numbers[depth - 1]) |> ignore
                else
                    x (depth + 1) basic
                    if reminder <> 0 then
                        result.Append(" ") |> ignore
                        x depth reminder
                
            x 0 number
            result.ToString()

