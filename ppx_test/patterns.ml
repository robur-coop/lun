type t = { foo: int; bar: string }
type n = { nested: t; baz: float }
type a = A
type b = B of int * int
type c = C of (int * int)
type u = X | Y | Z
type v = { mutable v: int }

let () =
  let open Lun in
  let v = { foo = 42; bar = "Hello World!" } in
  Fmt.pr "%d\n" (get [%lun? {foo; _}] v) ;
  Fmt.pr "%s\n" (get [%lun? {bar; foo = _}] v) ;
  let v = { nested = v; baz = 0.1 } in
  Fmt.pr "%d\n" (get [%lun? {nested = { foo ; _}; baz = _}] v) ;
  Fmt.pr "%f\n" (get [%lun? {baz; _}] v) ;
  assert (get [%lun? A] A = ()) ;
  assert (get [%lun? B(x,y)] (B (1, 2)) = (1, 2)) ;
  assert (get [%lun? C x] (C (1, 2)) = (1, 2)) ;
  assert (get [%lun? X] X = ()) ;
  assert (get_opt [%lun? Y] X = None) ;
  assert (get_opt [%lun? Z] Z = Some ()) ;
  Fmt.pr "%d\n" (get [%lun? B(x,_)] (B (1, 2))) ;
  Fmt.pr "%d\n" (get [%lun? B(_,x)] (B (1, 2))) ;
  let v0 = { v = 0 } in
  let v1 = setf ~f:succ [%lun? {v}] v0 in
  assert (v0.v = 0) ;
  assert (v1.v = 1) ;
  v1.v <- 2 ;
  assert (v0.v = 0) ;
  assert (v1.v = 2)
