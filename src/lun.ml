type (-'s, +'t, +'a, -'b) s = {
    f: 'r. ('a -> ('b -> 'r) -> 'r) -> 's -> ('t -> 'r) -> 'r
}
[@@unboxed]

type (-'s, +'t, +'a, -'b) t = unit -> ('s, 't, 'a, 'b) s

exception Undefined

let never _ = raise Undefined

let lense f g =
  let f k t r = k (f t) (fun b -> r (g t b)) in
  { f }

let prism f g =
  let f k s r =
    let ok x = k x (fun b -> r (f b)) and error = r in
    Result.fold (g s) ~error ~ok
  in
  { f }

let optional f g =
  let f k s r =
    let ok x = k x (fun b -> r (f s b)) and error = r in
    Result.fold (g s) ~error ~ok
  in
  { f }

let get f t = (f ()).f (fun v _ -> v) t never
let get_opt f t = (f ()).f (fun v _ -> Some v) t (Fun.const Option.none)
let setf o ~f t = (o ()).f (fun a rf -> rf (f a)) t (fun r -> r)
let set o v = setf o ~f:(fun _ -> v)

let id () = lense Fun.id (fun _ x -> x)
let ( >> ) f g () = { f = (fun z -> (f ()).f ((g ()).f z)) }

let fst () = { f = (fun k (a, x) r -> k a (fun b -> r (b, x))) }
let snd () = { f = (fun k (x, b) r -> k b (fun a -> r (x, a))) }

let some () =
  prism Option.some @@ function
  | Some x -> Result.ok x
  | None as x -> Result.error x

let rec set_nth i l elt = match i, l with
  | _, [] -> []
  | 0, _h :: t -> elt :: t
  | n, h :: t -> h :: set_nth (n-1) t elt
let get_nth i l =
  match List.nth_opt l i with Some v -> v | None -> raise Undefined
let nth i () = lense (get_nth i) (set_nth i)
