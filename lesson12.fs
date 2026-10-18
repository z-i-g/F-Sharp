// 34.1
let rec upto = function
    | n when n <= 0 -> []
    | n -> upto (n - 1) @ [n]

// 34.2
let rec dnto = function
    | n when n <= 0 -> []
    | n -> n :: dnto (n - 1)

// 34.3
let rec evenn = function
    | n when n <= 0 -> []
    | n -> evenn (n - 1) @ [2 * (n - 1)]