namespace NlpTools

module Number2Words =
    open System.Text

    let small = [|
        [|
            "один";
            "два";
        |];
        [|
            "одна";
            "дві";
        |];
    |]

    let larger = [|
        "три";
        "чотири";
        "п'ять";
        "шість";
        "сім";
        "вісім";
        "дев'ять";
        "десять";
        "одинадцять";
        "дванадцять";
        "тринадцять";
        "чотирнадцять";
        "п'ятнадцять";
        "шістнадцять";
        "сімнадцять";
        "вісімнадцять";
        "дев'ятнадцять";
    |]

    let tens_text = [|
        "двадцять"
        "тридцять"
        "сорок"
        "п'ятдесят"
        "шістдесят"
        "сімдесят"
        "вісімдесят"
        "дев'яносто"
    |]

    type Gender =
    | Feminine
    | Masculine

    let toText lang number form gender case =
        if lang <> "uk" then failwith "Only Ukrainian supported"
        if number = 0 
            then "нуль"
        else
            let tens = number / 10 % 10
            let ones = number % 10
            let result = StringBuilder()
            if tens >= 2 then
                result.Append(tens_text[tens - 2]) |> ignore
                if ones > 0 then
                    result.Append(" ") |> ignore
            elif tens = 1 then
                result.Append(larger[number - 3]) |> ignore

            if ones > 0 && tens <> 1 then
                if ones <= 2 then
                    let small_num = 
                        match gender with
                        | Masculine -> small[0]
                        | Feminine -> small[1]
                    result.Append(small_num[ones - 1]) |> ignore
                else
                    result.Append(larger[ones - 3]) |> ignore

            result.ToString()