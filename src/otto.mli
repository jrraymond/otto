val logger_fun : (string -> unit) ref
val default_logger : string -> unit
type ('a, 'b) either = Left of 'a | Right of 'b
val from_either : ('a -> 'b) -> ('c -> 'b) -> ('a, 'c) either -> 'b
val test :
  ?msg:string ->
  ?timeout:int ->
  ('a -> 'b) ->
  'a ->
  (exn, 'c) either ->
  ((exn, 'b) either -> 'd) ->
  ('a -> string) -> ('c -> string) -> unit -> unit
val otest :
  ?msg:string ->
  ?timeout:int ->
  ('a -> 'b) ->
  'a ->
  'b ->
  ('b -> 'b -> bool) -> ('a -> string) -> ('b -> string) -> unit -> unit
val ftest :
  ?msg:string ->
  ?timeout:int ->
  ('a -> 'b) ->
  'a ->
  exn ->
  (exn -> exn -> bool) -> ('a -> string) -> ('b -> string) -> unit -> unit
val tgroup : ?msg:string -> (unit -> 'a) list -> unit -> unit
val tgroup_h :
  ?msg:string ->
  'a -> 'b -> 'c -> ('a -> 'b -> 'c -> unit -> 'd) list -> unit -> unit
val run_tests : (unit -> 'a) -> (string -> unit) -> 'a
