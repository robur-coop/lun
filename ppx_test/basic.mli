type t = { foo: int; bar: string } [@@deriving lun]
type n = { nested: t; baz: float } [@@deriving lun]
type a = A [@@deriving lun]
type b = B of int * int [@@deriving lun]
type c = C of (int * int) [@@deriving lun]
type r = A of { foo: int } | B of { a: int; b: int } [@@deriving lun]
