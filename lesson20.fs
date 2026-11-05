// 49.5.1
let even_seq = Seq.initInfinite (fun i -> (i + 1) * 2)

// 49.5.2
let fac_seq = Seq.initInfinite(let rec fac x = 
                                  match x with
                                  |0 ->1
                                  |x -> x * fac(x-1)
                               fun x -> fac x)

// 49.5.
let seq_seq = Seq.initInfinite (fun i -> if i % 2 = 0 then i / 2 else -(i / 2 + 1))