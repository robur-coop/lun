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

let get f t = (f ()).f (fun v _ -> v) t never
let get_opt f t = (f ()).f (fun v _ -> Some v) t (Fun.const Option.none)
let setf o ~f t = (o ()).f (fun a rf -> rf (f a)) t (fun r -> r)
let set o v = setf o ~f:(fun _ -> v)
let ( >> ) f g () = { f = (fun z -> (f ()).f ((g ()).f z)) }
let fst () = { f = (fun k (a, x) r -> k a (fun b -> r (b, x))) }
let snd () = { f = (fun k (x, b) r -> k b (fun a -> r (x, a))) }

let some () =
  prism Option.some @@ function
  | Some x -> Result.ok x
  | None as x -> Result.error x
