type a = { v: string } [@@deriving lun]
type b = { a: a } [@@deriving lun]
type c = { b: b; c: float } [@@deriving lun]

let update_v c v = Lun.(set (c_b >> b_a >> a_v)) v c

let () =
  let v0 = { c = 0.; b = { a = { v = "Hello World!" } } } in
  print_endline (update_v v0 "Salut!").b.a.v
