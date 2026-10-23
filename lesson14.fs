// 40.1
let rec sum (p, xs) =
    match xs with
    | [] -> 0
    | x :: tail ->
        let restSum = sum (p, tail)
        if p x then x + restSum else restSum

// 40.2.1
let rec count (xs, n) =
    match xs with
    | [] -> 0
    | x :: tail ->
        let restCount = count (tail, n)
        if x = n then 1 + restCount else restCount

// 40.2.2
let rec insert (xs, n) =
    match xs with
    | [] -> [n]
    | x :: tail ->
        if n <= x then
            n :: x :: tail
        else
            x :: insert (tail, n)

// 40.2.3
let rec intersect (xs1, xs2) =
    match (xs1, xs2) with
    | ([], _) | (_, []) -> []
    | (x :: tail1, y :: tail2) ->
        if x = y then
            x :: intersect (tail1, tail2)
        elif x < y then
            intersect (tail1, xs2)
        else
            intersect (xs1, tail2)

// 40.2.4
let rec plus (xs1, xs2) =
    match (xs1, xs2) with
    | ([], _) -> xs2
    | (_, []) -> xs1
    | (x :: tail1, y :: tail2) ->
        if x <= y then
            x :: plus (tail1, xs2)
        else
            y :: plus (xs1, tail2)

// 40.2.5
let rec minus (xs1, xs2) =
    match (xs1, xs2) with
    | ([], _) -> []
    | (xs1, []) -> xs1
    | (x :: tail1, y :: tail2) ->
        if x = y then
            minus (tail1, tail2)
        elif x < y then
            x :: minus (tail1, xs2)
        else
            minus (xs1, tail2)

// 40.3.1
let smallest xs =
    match xs with
    | [] -> None
    | x :: _ -> Some x

// 40.3.2
let rec delete (n, xs) =
    match xs with
    | [] -> []
    | x :: tail ->
        if x = n then 
            tail
        elif x > n then 
            xs
        else 
            x :: delete (n, tail)

// 40.3.3
let rec sort = function
    | [] -> []
    | x :: tail -> 
        insert (sort tail, x)


// 40.4
let rec revrev = function
    | [] -> []
    | head :: tail -> 
        let rec rev = function
            | [] -> []
            | y :: ys -> (rev ys) @ [y]
        (revrev tail) @ [rev head]