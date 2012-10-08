open Printf
open Scanf

let ( |> ) x f = f x

let read_case ib =
  let n, k = bscanf ib "%d %d\n" (fun n k -> n, k) in
  let rec loop n acc =
    if n = 0 then List.rev acc else
    let p, c = bscanf ib "%d/%d %d\n" (fun a b c ->
      let p = float a /. float b in p, c) in
    loop (n - 1) ((p, c) :: acc)
  in
  n, k, loop n []

let solve (n, k, act) =
  let act = List.sort (fun (a, _) (b, _) -> compare a b) act in

  (* activities must be performed from noisier to quieter *)

  let quiet = Array.make k 0. in
  let i = ref (k - 1) in
  act |> List.iter (fun (p, c) ->
    for j = 1 to c do
      if !i >= 0 then (quiet.(!i) <- p; decr i)
    done);

  let noisy = Array.make k 0. in
  let i = ref 0 in
  (List.rev act) |> List.iter (fun (p, c) ->
    for j = 1 to c do
      if !i < k then (noisy.(!i) <- p; incr i)
    done);

  (* use quietest as a baseline *)

  let w = Array.make (k + 1) 0. in (* stays awake after index *)
  let s = Array.make (k + 1) 0. in (* stays asleep after index *)
  let f = Array.make (k + 1) 0. in (* falls asleep once after index *)

  w.(k) <- 1.;
  s.(k) <- 1.;
  f.(k) <- 0.;

  for i = k - 1 downto 0 do
    w.(i) <- w.(i + 1) *. quiet.(i);
    s.(i) <- s.(i + 1) *. (1. -. quiet.(i));
    f.(i) <- f.(i + 1) *. quiet.(i) +. s.(i + 1) *. (1. -. quiet.(i))
  done;

  let m = ref (1.0 -. (w.(0) +. f.(0))) in

  let w' = ref 1. in (* stays awake before index *)
  let f' = ref 0. in (* falls asleep once before index *)
  let i0 = ref (-1) in

  for i = 0 to k - 1 do
    (* substitute de i-th noisy activity for the i-th quiet one *)
    f' := (!f' +. !w') *. (1. -. noisy.(i));
    w' := !w' *. noisy.(i);
    let wf = !w' *. w.(i + 1) in
    let ff = !w' *. f.(i + 1) +. !f' *. s.(i + 1) in
    let p = 1.0 -. (wf +. ff) in
    if p < !m then i0 := i;
    m := min !m p;
  done;
  (*printf "i0 = %d\n" !i0;*)
  abs_float !m

let solve_all fname =
  let ib = Scanning.from_file fname in
  let t = bscanf ib "%d\n" (fun t -> t) in
  for i = 1 to t do
    read_case ib |> solve |> (fun r -> printf "Case #%d: %f\n%!" i r)
  done

let () = if not !Sys.interactive then solve_all Sys.argv.(1)

