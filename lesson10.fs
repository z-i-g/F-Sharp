type TimeOfDay = { hours: int; minutes: int; f: string }

let (.>.) x y =
    let toMinutes t =
        let h = if t.f = "PM" then t.hours + 12 else t.hours
        h * 60 + t.minutes
    toMinutes x > toMinutes y