// bind
let myBind (binder) (opt) = 
    match opt with
    | None -> None
    | Some x -> binder x

// contains
let myContains (value) (opt) = 
    match opt with
    | Some x -> x = value
    | None -> false

// count
let myCount (opt) = 
    match opt with
    | Some x -> 1
    | None -> 0

// defaultValue
let myDefaultValue (value) (opt) =
    match opt with
    | None -> value
    | Some x -> x

// defaultWith
let myDefaultWith (defThunk) (opt) = 
    match opt with
    | Some x -> x
    | None -> defThunk

// exists
let myExists (pred) (opt) = 
    match opt with
    | Some x -> pred x
    | None -> false

// filter
let myFilter (pred) (opt) = 
    match opt with
    | Some x -> if pred x then Some(x) else None
    | None -> None

// flatten
let myFlatten (opt) =
    match opt with
    | Some x -> x
    | None -> None

// fold
let myFold (folder) (state) (opt) = 
    match opt with
    | None -> state
    | Some x -> folder (x) (state)

// forall
let myForAll (pred) (opt) = 
    match opt with
    | None -> true
    | Some x -> pred x

// map
let myMap (mapping) (opt) = 
    match opt with
    | None -> None
    | Some x -> mapping x

// map2
let myMap2 (mapping) (opt1) (opt2) = 
    match (opt1, opt2) with
    | (None, None) -> None
    | (Some x, None) -> None
    | (None, Some x) -> None
    | (Some x, Some y) -> mapping x y

// orElse
let myOrElse (ifNone) (opt) = 
    match opt with 
    | Some x -> x
    | None -> ifNone

// toArray
let myToArray (opt) = 
    match opt with
    | None -> [||]
    | Some x -> [|x|]

// toList
let myToList (opt) = 
    match opt with
    | None -> []
    | Some x -> [x]