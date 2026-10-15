type F = 
  | AM
  | PM

type TimeOfDay = { hours : int; minutes : int; f: F }

let transform (x: TimeOfDay) =
    let shift = function
      | AM -> 0
      | PM -> 12
    (x.hours + shift x.f) * 60 + x.minutes

let (.>.) x y = transform x > transform y