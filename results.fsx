// bind
let myBind binder result = 
    match result with
    | Error e -> Error e 
    | Ok x -> binder x

// map
let myMap mapping result = 
    match result with
    | Error e -> Error e
    | Ok x -> Ok(mapping x)

// mapError
let myMapError mapping result = 
    match result with
    | Ok x -> Ok x
    | Error e -> Error(mapping e)

