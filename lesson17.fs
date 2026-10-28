// 43.3
let try_find key m = 
    match (m : Map<'k, 'v>).TryGetValue(key) with
    | true, value -> Some value
    | false, _    -> None