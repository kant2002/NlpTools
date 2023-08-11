namespace NlpTools

module Number2Words =

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
    |]

    type Gender =
    | Feminine
    | Masculine

    let toText lang number form gender case =
        if lang <> "uk" then failwith "Only Ukrainian supported"
        if number = 0 
            then "нуль"
        else
            if number <= 2 then
                let small_num = 
                    match gender with
                    | Masculine -> small[0]
                    | Feminine -> small[1]
                small_num[number - 1]
            else
                larger[number - 3]