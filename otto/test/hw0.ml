(* Implementation of the homework *)

let rec fibonacci i =
  match i with
  | 0 -> 0
  | 1 -> 1
  | _ -> fibonacci (i - 1) + fibonacci (i - 2)


let rec factorial i = 
  if i < 2 then 1 else i * factorial (i - 1)
