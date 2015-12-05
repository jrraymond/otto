(* Examples / Tests *)
open Otto

(* give string representation of \-terms *)
let rec string_of_term tm =
  match tm with
  | Hw1.TmVar x -> x
  | Hw1.TmApp (t1, t2) ->
      "("^string_of_term t1^" "^string_of_term t2^")"
  | Hw1.TmAbs(x,t) -> "\\"^x^"."^string_of_term t;;

let rec string_of_dbterm tm = 
  match tm with
  | Hw1.DbTmVar i -> string_of_int i
  | Hw1.DbTmApp (t1, t2) ->
      "(" ^ string_of_dbterm t1 ^ " " ^ string_of_dbterm t2 ^ ")"
  | Hw1.DbTmAbs t -> "\\." ^ string_of_dbterm t

let index_of =
  let rec index_of_h i y xs =
    match xs with
    | [] -> raise (Match_failure ("index_of_h", 0, 0))
    | (x::xs') -> if x = y then i else index_of_h (i+1) y xs' in
  index_of_h 0

let rec lam2db ctx tm = 
  match tm with
  | Hw1.TmVar x -> Hw1.DbTmVar (index_of x ctx)
  | Hw1.TmAbs (x,t1) -> Hw1.DbTmAbs (lam2db (x::ctx) t1)
  | Hw1.TmApp (t1,t2) -> Hw1.DbTmApp (lam2db ctx t1, lam2db ctx t2)

let rec nubs xs = 
  (match xs with
  | [] -> []
  | (x::xs) -> x :: (nubs (List.filter ((<>) x) xs)));;

let free_vars t = 
  let rec fv t =
    (match t with 
    | Hw1.TmVar x -> [x]
    | Hw1.TmAbs (x,t1) -> List.filter ((<>) x) (fv t1)
    | Hw1.TmApp (t1,t2) -> (fv t1) @ (fv t2))
  in nubs (fv t)

let alpha_eq t1 t2 = 
  lam2db (free_vars t1) t1 = lam2db (free_vars t2) t2

let suite = 
  tgroup ~msg:"Example tests"
  [ tgroup ~msg:"Homework 0"
    [ tgroup_h ~msg:"fibonacci" (=) string_of_int string_of_int
      [ otest Hw0.fibonacci 0 0;
        otest Hw0.fibonacci 1 1;
        otest Hw0.fibonacci 2 1;
        otest Hw0.fibonacci 3 2;
        otest Hw0.fibonacci 4 3;
        otest Hw0.fibonacci 5 5;
        otest Hw0.fibonacci 6 8
      ];
      tgroup_h ~msg:"factorial" (=) string_of_int string_of_int
      [ otest Hw0.factorial 0 1;
        otest Hw0.factorial 1 1;
        otest Hw0.factorial 2 2;
        otest Hw0.factorial 3 6;
        otest Hw0.factorial 4 24;
        otest Hw0.factorial 5 120
      ]
    ];
    tgroup ~msg:"Homework 1"
    [ tgroup_h ~msg:"db2lam" alpha_eq string_of_dbterm string_of_term
      [ otest (Hw1.db2lam ["x"]) (Hw1.DbTmVar 0) (Hw1.TmVar "x");
        otest (Hw1.db2lam []) (Hw1.DbTmAbs (Hw1.DbTmVar 0)) (Hw1.TmAbs ("x", Hw1.TmVar "x"));
        otest (Hw1.db2lam ["x"])
          (Hw1.DbTmApp (Hw1.DbTmAbs (Hw1.DbTmVar 0), Hw1.DbTmVar 0))
          (Hw1.TmApp (Hw1.TmAbs ("x", Hw1.TmVar "x"), Hw1.TmVar "x"));
      ];
      tgroup_h ~msg:"lam2db" (=) string_of_term string_of_dbterm
      [ otest (Hw1.lam2db ["x"]) (Hw1.TmVar "x") (Hw1.DbTmVar 0);
        otest (Hw1.lam2db [])
          (Hw1.TmAbs ("x", Hw1.TmVar "x"))
          (Hw1.DbTmAbs (Hw1.DbTmVar 0));
        otest (Hw1.lam2db ["x"])
          (Hw1.TmApp (Hw1.TmAbs ("x", Hw1.TmVar "x"), Hw1.TmVar "x"))
          (Hw1.DbTmApp (Hw1.DbTmAbs (Hw1.DbTmVar 0), Hw1.DbTmVar 0));
      ];
      tgroup_h ~msg:"bad will fail" (=) string_of_int string_of_int
      [ otest Hw1.bad_will_fail 0 0 ];
      tgroup_h ~msg:"bad will loop" (=) string_of_int string_of_int
      [ otest Hw1.bad_will_loop 0 0 ];
      tgroup_h ~msg:"should fail" (=) string_of_int string_of_int
      [ otest Hw1.should_fail 0 0 ];
    ]
  ]
      


let _ = run_tests suite default_logger
