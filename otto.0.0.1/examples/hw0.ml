(* Implementation of the homework *)

let rec fibonacci i =
  match i with
  | 0 -> 0
  | 1 -> 1
  | _ -> fibonacci (i - 1) + fibonacci (i - 2)


let rec factorial i = i * factorial i
