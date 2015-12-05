val default_logger : string -> unit
val otest :
  ?msg:string ->
  ?timeout:int ->
  ('a -> 'b) ->
  'a ->
  'c ->
  ('c -> 'b -> bool) -> ('a -> string) -> ('c -> string) -> unit -> unit
val ftest :
  ?msg:string ->
  ?timeout:int ->
  ('a -> 'b) ->
  'a ->
  'c ->
  ('c -> exn -> bool) -> ('a -> string) -> ('c -> string) -> unit -> unit
val tgroup : ?msg:string -> (unit -> 'a) list -> unit -> unit
val tgroup_h :
  ?msg:string ->
  'a -> 'b -> 'c -> ('a -> 'b -> 'c -> unit -> 'd) list -> unit -> unit
val run_tests : (unit -> 'a) -> (string -> unit) -> 'a
