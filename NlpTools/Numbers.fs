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

    let hundreds_text = [|
        "сто"
        "двісті"
        "триста"
        "чотириста"
        "п'ятсот"
        "шістсот"
        "сімсот"
        "вісімсот"
        "дев'ятсот"
    |]

    let large_numbers = [|
        [| "тисяча"; "тисячи"; "тисяч" |];
        [| "мільйон"; "мільйона"; "мільйонів" |];
        [| "мільярд"; "мільярда"; "мільярдів" |];
        [| "трильйон"; "трильйона"; "трильйонів" |];
        [| "квадрильйон"; "квадрильйона"; "квадрильйонів" |];
        [| "квінтильйон"; "квінтильйона"; "квінтильйонів" |];
        [| "секстильйон"; "секстильйона"; "секстильйонів" |];
        [| "септильйон"; "септильйона"; "септильйонів" |];
        [| "октильйон"; "октильйона"; "октильйонів" |];
        [| "нонільйон"; "нонільйона"; "нонільйонів" |];
    |]

    type Gender =
    | Feminine
    | Masculine

    let private numberForm number =
        let target = number % 100
        if target = 1 then
            0
        elif target <= 4 then
            1
        else
            2

    let private printThousands (result: StringBuilder) number gender =
        let hundreds = (number / 100) % 10
        let tens = (number / 10) % 10
        let ones = number % 10
        if hundreds > 0 then
            result.Append(hundreds_text[hundreds - 1]) |> ignore
            if tens > 0 || ones > 0 then
                result.Append(" ") |> ignore
        if tens >= 2 then
            result.Append(tens_text[tens - 2]) |> ignore
            if ones > 0 then
                result.Append(" ") |> ignore
        elif tens = 1 then
            result.Append(larger[tens * 10 + ones - 3]) |> ignore

        if ones > 0 && tens <> 1 then
            if ones <= 2 then
                let small_num = 
                    match gender with
                    | Masculine -> small[0]
                    | Feminine -> small[1]
                result.Append(small_num[ones - 1]) |> ignore
            else
                result.Append(larger[ones - 3]) |> ignore

    let toText lang (number: int64) form gender case =
        if lang <> "uk" then failwith "Only Ukrainian supported"
        if number = 0 
            then "нуль"
        else
            let result = StringBuilder()
            let rec x depth num g =
                let reminder = (num % 1000L) |> int
                let basic = num / 1000L
                if (basic = 0) then
                    printThousands result reminder g
                    if depth > 0 then
                        result.Append(" ") |> ignore
                        let form = numberForm reminder
                        result.Append(large_numbers[depth - 1][form]) |> ignore
                else
                    x (depth + 1) basic (if depth = 0 then Feminine else Masculine)
                    if reminder <> 0 then
                        result.Append(" ") |> ignore
                        x depth reminder g
                
            x 0 number gender
            result.ToString()
    //let toText lang (number: int32) form gender case =
    //    toText lang (number |> int64) form gender case