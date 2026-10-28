// 47.4.1
let f n = 
    let mutable res = 1
    let mutable i = 1
    while i <= n do
        res <- res * i
        i <- i + 1
    res


// 47.4.2
let fibo n = 
    if n = 0 then 0
    elif n = 1 then 1
    else
        let mutable prev = 0
        let mutable curr = 1
        let mutable i = 2
        while i <= n do
            let next = prev + curr
            prev <- curr
            curr <- next
            i <- i + 1
        curr