module ICPC
open System

let listOfValidChars c = 
    match c with
    | 'a' -> false | 'b' -> false | 'c' -> false | 'd' -> false | 'e' -> false | 'f' -> false | 'g' -> false | 'h' -> false | 'i' -> false | 'j' -> false
    | 'k' -> false | 'l' -> false | 'm' -> false | 'n' -> false | 'o' -> false | 'p' -> false | 'q' -> false | 'r' -> false | 's' -> false | 't' -> false
    | 'u' -> false | 'v' -> false | 'w' -> false | 'x' -> false | 'y' -> false | 'z' -> false | ',' -> false | '.' -> false | ' ' -> false | _ -> true

let rec validChars (input: char list) =
    match (Seq.toList input) with
    | head::tail -> match (listOfValidChars head) with
                    | true -> validChars tail
                    | false -> None
    | [] -> Some true

let rec lengthWord (input: string list) =
    match input with
    | head::tail -> match (head.Length > 1) with
                    | true -> lengthWord tail
                    | false -> None
    | [] -> Some true

let isValidString input = 
    match (lengthWord (Seq.toList (input.ToString().Split(' ')))) with
    | Some true -> let rec allWords input =
                       match (Seq.toList input) with 
                       | head::tail -> match (validChars head) with
                                       | false -> allWords tail
                                       | true -> None
                       | [] -> Some true

                   allWords ((input.ToString().Split(' ')))

let commaSprinkler input =
    isValidString input

let rivers input =
    failwith "Not implemented"

[<EntryPoint>]
let main argv =
    0 // return an integer exit code
