// 39.1
let rec rmodd = function
    | _ :: head2 :: tail -> head2 :: rmodd tail
    | _ -> []

// 39.2
let rec del_even = function 
    | [] -> []
    | head :: tail ->
        if head % 2 = 0 then del_even tail
        else head :: del_even tail

// 39.3
let rec multiplicity x xs = 
    match xs with
    | [] -> 0
    | head :: tail -> 
        if head = x then 1 + multiplicity x tail 
        else multiplicity x tail

// 39.4
let rec split = function
    | [] -> ([], [])
    | [x] -> ([x], [])
    | x1 :: x2 :: tail -> 
        let (xs1, xs2) = split tail
        (x1 :: xs1, x2 :: xs2)

// 39.5
let rec zip (xs1, xs2) = 
    match (xs1, xs2) with
    | ([], []) -> []
    | (head1 :: tail1, head2 :: tail2) -> (head1, head2) :: zip (tail1, tail2)
    | _ -> failwith "Списки имеют разную длину"