// 16.1
let notDivisible (n,m) = m % n = 0

// 16.2
let prime (n: int) : bool =
    if n <= 1 then false
    else
        let rec check d =
            if d * d > n then true
            elif n % d = 0 then false 
            else check (d + 1)
        check 2