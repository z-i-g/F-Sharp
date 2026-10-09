// 23.4.1
let (.+.) x y =
    let (g1, s1, c1) = x
    let (g2, s2, c2) = y
    let totalCopper = (g1 + g2) * 240 + (s1 + s2) * 12 + (c1 + c2)
    (totalCopper / 240, (totalCopper % 240) / 12, totalCopper % 12)

let (.-.) x y =
    let (g1, s1, c1) = x
    let (g2, s2, c2) = y
    let totalCopper = (g1 - g2) * 240 + (s1 - s2) * 12 + (c1 - c2)
    (totalCopper / 240, (totalCopper % 240) / 12, totalCopper % 12)


// 23.4.2
let (.+) x y =
    let (a, b) = x
    let (c, d) = y
    (a + c, b + d)

let (.-) x y =
    let (a, b) = x
    let (c, d) = y
    (a - c, b - d)

let (.*) x y =
    let (a, b) = x
    let (c, d) = y
    (a * c - b * d, b * c + a * d)

let (./) x y =
  let (a, b) = y
  x .* (a/(a*a+b*b), -b/(a*a+b*b))
