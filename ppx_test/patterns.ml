type t =
  | Add of t * t
  | Int of int

let f = [%lun? Add (x, y)]

let e = [%lun? Add (_, Int v) when v > 10]

type x = { x : int }

let x = [%lun? { x }]
