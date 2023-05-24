type t = { foo: int; bar: string } [@@deriving lun]
type n = { nested: t; baz: float } [@@deriving lun]
type a = A [@@deriving lun]
type b = B of int * int [@@deriving lun]
type c = C of (int * int) [@@deriving lun]
type u = X | Y | Z [@@deriving lun]
type v = { mutable v: int } [@@deriving lun]

let () =
  let open Lun in
  let v = { foo = 42; bar = "Hello World!" } in
  Fmt.pr "%d\n" (get t_foo v) ;
  Fmt.pr "%s\n" (get t_bar v) ;
  let v = { nested = v; baz = 0.1 } in
  Fmt.pr "%d\n" (get (n_nested >> t_foo) v) ;
  Fmt.pr "%f\n" (get n_baz v) ;
  assert (get a_A A = ()) ;
  assert (get b_B (B (1, 2)) = (1, 2)) ;
  assert (get c_C (C (1, 2)) = (1, 2)) ;
  assert (get u_X X = ()) ;
  assert (get_opt u_Y X = None) ;
  assert (get_opt u_Z Z = Some ()) ;
  Fmt.pr "%d\n" (get (b_B >> fst) (B (1, 2))) ;
  Fmt.pr "%d\n" (get (b_B >> snd) (B (1, 2))) ;
  let v0 = { v = 0 } in
  let v1 = setf ~f:succ v_v v0 in
  assert (v0.v = 0) ;
  assert (v1.v = 1) ;
  v1.v <- 2 ;
  assert (v0.v = 0) ;
  assert (v1.v = 2)
