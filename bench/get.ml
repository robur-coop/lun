open Bechamel
open Toolkit

type t = { v: int } [@@deriving lun]

let handwritten_get { v } = v
let v0 = { v = 0 }

let test0 =
  Test.make ~name:"Lun.get" @@ Staged.stage
  @@ fun () ->
  let _ = Lun.get t_v v0 in
  ()

let test1 =
  Test.make ~name:"handwritten get"
  @@ Staged.stage
  @@ fun () ->
  let _ = handwritten_get v0 in
  ()

let test = Test.make_grouped ~name:"get" [ test0; test1 ]

let benchmark () =
  let ols =
    Analyze.ols ~bootstrap:0 ~r_square:true ~predictors:Measure.[| run |]
  in
  let instances = Instance.[ monotonic_clock ] in
  let cfg =
    Benchmark.cfg ~limit:3000 ~quota:(Time.second 1.0) ~kde:(Some 1000) ()
  in
  let raw_results = Benchmark.all cfg instances test in
  let results =
    List.map (fun instance -> Analyze.all ols instance raw_results) instances
  in
  let results = Analyze.merge ols instances results in
  (results, raw_results)

let () =
  let results =
    let open Bechamel_js in
    emit ~dst:(Channel stdout) (Fun.const (Ok ())) ~compare:String.compare
      ~x_label:Measure.run
      ~y_label:(Measure.label Instance.monotonic_clock)
      (benchmark ())
  in
  match results with Ok () -> () | Error (`Msg msg) -> failwith msg
