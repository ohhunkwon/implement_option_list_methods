// append
let rec myAppend list1 list2 =
    match (list1, list2) with
    | [], yr -> yr
    | x::xr, y -> x::myAppend xr y

// choose
let rec myChoose (chooser:'a -> 'b option) (xs:'a list): 'b list =
    match xs with
    | [] -> []
    | x::xr -> 
        let c = chooser x
        match c with
        | Some b -> b::(myChoose chooser xr )
        | None -> myChoose chooser xr

// collect
let rec myCollect mapping list = 
    match list with
    | [] -> []
    | x::xr -> mapping x @ myCollect mapping xr

// [1..4] |> myCollect (fun x -> [1..x])
// Better way to implement collect is perhaps:
// let myCollectVer2 mapping xs =
//     let rec prepend res xs = function
//         | [] -> loop res xs
//         | y::ys -> prepend (y::res) xs ys
//     and loop res = function
//         | [] -> List.rev res
//         | x::xs -> prepend res xs (mapping x)
//     loop [] xs

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
    | [] -> []
    | x::xs -> x @ myConcat(xs)

// let input = [ [1;2];
//                [3;4;5];
//                [6;7;8;9] ]
// input |> myConcat  // evaluates [1; 2; 3; 4; 5; 6; 7; 8; 9]

// contains
let rec myContains value source =
    match source with
    | x::_ when x = value -> true
    | _::xr -> myContains value xr
    | [] -> false

// empty
let myEmpty = []

// exists
let rec myExists predicate xs =
    match xs with
    | [] -> false
    | x::_ when predicate x -> true
    | _::xs -> myExists predicate xs

// let input = [1, "Kirk"; 2, "Spock"; 3, "Kenobi"] 
// input |> myExists (fun x -> x = (3, "Kenobi"))  // evaluates true
// input |> myExists (fun (n, name) -> n > 5)      // evaluates false

// filter
let rec myFilter predicate list = 
    match list with
    | x::xs when predicate x -> x :: myFilter predicate xs
    | _::xr -> myFilter predicate xr 
    | [] -> []

// let input = [1, "Luke"; 2, "Kirk"; 3, "Kenobi"; 4, "Spock"]
// let isComingFromStarTrek (x,_) = x % 2 = 0
// input |> myFilter isComingFromStarTrek

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

// rev
let myRev list =
    let rec loop acc = function
        | []           -> acc
        | head :: tail -> loop (head :: acc) tail
    loop [] list

// scan
let rec myScan folder state list = 
    match list with
    | [] -> [state]
    | x::xr -> 
        let stateUpdated = folder state x
        state::myScan folder stateUpdated xr

// type Charge =
//     | In of int
//     | Out of int

// let inputs = [In 1; Out 2; In 3]

// (0, inputs) ||> myScan (fun acc charge ->
//     match charge with
//     | In i -> acc + i
//     | Out o -> acc - o)

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

// unzip
let rec myUnzip = function
    |    []          ->  ([],[])
    |    (x,y)::rest ->
               let (xs,ys) = myUnzip rest
               (x::xs,y::ys)