# Lun(ette), optics in OCaml

This small library proposes values (optics) allowing to access (and modify)
fields of a record or to project values (or modify them) of a constructor in
OCaml. These values can be composed together and thus facilitate the access and
modification of certain fields in _nested_ records.

As an example, here is a "nested" record `c`:
```ocaml
type a =
  { v : string }
type b =
  { a : a }
type c =
  { b : b
  ; c : float }
```

It can be annoying to update `a` from `c`:
```ocaml
let update_v : c -> string -> c = fun c v ->
  { c with b= { c.b with a= { v }}}
```

Lun(ette) thus proposes values which are called "optics" allowing to refer to
the field of a record. The particularity of these values is that they can be
composed between them:
```ocaml
let a_v () = Lun.lense (fun { v } -> v) (fun _ v -> { v })
let b_a () = Lun.lense (fun { a } -> a) (fun _ a -> { a })
let c_b () = Lun.lense (fun { b; _ } -> b) (fun c b -> { c with b })

let update_v c v =
  Lun.(set (c_b >> b_a >> a_v)) v c
```

## `ppx_lun`

As you can see, the creation of these optics is quite simple. That's why
Lun(ette) also offers a ppx that generates these values for you:
```ocaml
type a =
  { v : string } [@@deriving lun]
type b =
  { a : a } [@@deriving lun]
type c =
  { b : b
  ; c : float } [@@deriving lun]

let update_v c v =
  Lun.(set (c_b >> b_a >> a_v)) v c
```

The user can then use this ppx in his/her library or in his/her executable by
adding this new field if he/she is a `dune` user:
```diff
+ (preprocess
+  (pps ppx_lun))
```

The user can also obtain the generated code directly from the tool provided in
our distribution:
```shell-session
$ cat >main.ml <<EOF
type t = { v : int } [@@deriving lun]
EOF
$ lun main.ml
type t = {
  v: int }[@@deriving lun]
include
  struct
    let _ = fun (_ : t) -> ()
    let t_v () =
      Lun.lense (fun vObwn1M -> vObwn1M.v) (fun _ -> fun v -> { v })
    let _ = t_v
  end[@@ocaml.doc "@inline"][@@merlin.hide ]
```

## How to us it?

Lun(ette) can be used for records but also for ADTs. We offer a fairly complete
documentation on the use of this library (with examples) as well as a
description of the use of ppx.
