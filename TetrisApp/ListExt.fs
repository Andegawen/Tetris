module ListExt
    let getRandomElement (random:System.Random) l =
        let pos = random.Next(List.length l - 1)
        l.[pos]