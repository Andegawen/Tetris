module ListExt
    let getRandomElement (random:System.Random) l =
        let pos = random.Next(List.length l - 1)
        l.[pos]
    (** safe list.maxBy option as result instead ArgumentException **)
    let maxBy (projection: 'a -> 'b) (list: 'a list) : 'a option =
         if list.IsEmpty
         then None
         else
            Some (List.maxBy projection list)
    let minBy (projection: 'a -> 'b) (list: 'a list) : 'a option =
         if list.IsEmpty
         then None
         else
            Some (List.minBy projection list)