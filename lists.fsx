// append
let rec myAppend list1 list2 =
    match (list1, list2) with
    | [], yr -> yr
    | x::xr, y -> x::myAppend xr y

// choose
let rec myChoose chooser list =
    match list with
    | [] -> []
    | x::xr -> chooser x :: myChoose chooser xr 

// collect
let rec myCollect mapping list = 
    match list with
    | [] -> []
    | x::xr -> mapping x :: myCollect mapping xr

// compareWith
let rec myCompareWith comparer list1 list2 =
    match (list1, list2) with
    | [], [] -> 0
    | [], yr -> -1
    | xr, [] -> 1
    | x::xr, y::yr -> 
        let compared = comparer x y
        if compared <> 0 then compared else myCompareWith comparer xr yr

// concat
let rec myConcat lists = 
    match lists with
    | [], yr -> yr
    | xr, [] -> xr
    | x::xr, y -> x :: myConcat (xr, y) 

// contains
let rec myContains value source =
    match source with
    | x::xr when x = value -> true
    | x::xr when x <> value -> myContains value xr 
    | [x] when x = value -> true
    | [x] when x <> value -> false
    | _ -> false

// empty
let myEmpty = []

// exists
let rec myExists predicate list =
    match list with
    | [] -> false
    | x::xr -> if predicate x then true else myExists predicate xr

// filter
let rec myFilter predicate list = 
    match list with
    | [] -> []
    | x::xr -> if predicate x then x else myFilter predicate xr 

// fold
let rec myFold folder state list =
    match list with
    | [] -> state
    | x::xr -> 
        let stateUpdated = folder state list
        myFold folder stateUpdated list

// forall 
let rec myForAll predicate list =
    match list with
    | [] -> true
    | x::xr -> if predicate x = false then false else myForAll predicate xr

// indexed
let myIndexed list = 
    let rec myBegin idx list =
        match list with
        | [] -> []
        | x::xr -> (idx,x) :: myBegin (idx+1) xr
    myBegin 0 list

// length
let myLength list = 
    let rec breakDown idx list =
        match list with
        | [] -> 0
        | x::xr -> breakDown (idx + 1) xr
    breakDown 0 list

// map 
let rec myMap mapping list = 
    match list with
    | [] -> []
    | x::xr -> mapping x :: myMap mapping xr

// rev (???)
let myRev list =
    let rec loop acc = function
        | []           -> acc
        | head :: tail -> loop (head :: acc) tail
    loop [] list

myRev [1;2;3;4;5]

// scan (???)

// tryFind
let rec myTryFind predicate list = 
    match list with
    | [] -> None
    | x::xr -> if predicate x then x else myTryFind predicate xr

// tryHead
let rec myTryHead list = 
    match list with
    | [] -> None
    | x::xr -> x

// zip
let rec myZip list1 list2 =
    match (list1, list2) with
    | x::xr, y::yr -> (x, y) :: myZip xr yr
    | _ -> []

// unzip (???)
let rec myUnzip = function
    |    []          ->  ([],[])
    |    (x,y)::rest ->
               let (xs,ys) = myUnzip rest
               (x::xs,y::ys)