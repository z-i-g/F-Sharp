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
            // Если индекс четный — делим на 2. Если нечетный — делим на 2, прибавляем 1 и ставим минус
            yield if i % 2 = 0 then i / 2 else -(i / 2 + 1)
            yield! loop (i + 1)
        }
    loop 0


printfn "Факториалы: %A" (fac_seq |> Seq.take 10 |> Seq.toList)
printfn "Числа: %A" (seq_seq |> Seq.take 10 |> Seq.toList)