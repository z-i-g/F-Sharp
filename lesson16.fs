// 42.3
let rec allSubsets n k =
    if k = 0 then
        Set.singleton Set.empty
    elif n < k || n < 1 then
        Set.empty
    else
        let withoutN = allSubsets (n - 1) k
        let withN = allSubsets (n - 1) (k - 1) |> Set.map (fun s -> Set.add n s)
        Set.union withoutN withN

