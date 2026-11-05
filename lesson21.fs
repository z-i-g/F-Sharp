// 50.2.1
let fac_seq =
    let rec loop i fac =
        seq {
            let nextFac = if i = 0 then 1 else fac * i
            yield nextFac
            yield! loop (i + 1) nextFac
        }
    loop 0 1

// 50.2.2
let seq_seq =
    let rec loop i =
        seq {
            yield if i % 2 = 0 then i / 2 else -(i / 2 + 1)
            yield! loop (i + 1)
        }
    loop 0