open Printf
open Scanf

let ( |> ) x f = f x

let read_case ib =
  let n = bscanf ib "%d\n" (fun n -> n) in
  let g = Array.make n (0, 0) in
  for i = 1 to n - 1 do
    bscanf ib "%d %d\n" (fun l r -> g.(i) <- (l, r))
  done;
  n, g

exception Exit of int

let brute (n, g) =
  let s = ref 0 in
  let c = ref 1 in
  try
    for i = 0 to n * (1 lsl n) do
      if !c = n then raise (Exit i) else
      let bit = 1 lsl !c in
      let s' = !s lxor bit in
      c := if !s land bit = 0 then fst g.(!c) else snd g.(!c);
      s := s'
    done;
    "Infinity"
  with
  | Exit i -> sprintf "%d" i

type t = Far | Near | Dead
type dir = L | R

let split (n, g) =

  (* Calculate distances from final clearing *)

  let dist = Array.make (n + 1) (n + 1) in
  dist.(n) <- 0;
  for i = 1 to n - 1 do
    for j = 1 to n - 1 do
      let l, r = g.(j) in
      dist.(j) <- min dist.(j) (min (1 + dist.(l)) (1 + dist.(r)))
    done
  done;

  (* Classify clearings into near, far, and dead ends *)

  let typ = Array.make (n + 1) Dead in
  let bitno = Array.make (n + 1) 0 in

  let s = Array.mapi (fun i d -> i, d) dist in
  Array.sort (fun (_, a) (_, b) -> compare a b) s;

  let k = ref n in
  while snd s.(!k) = n + 1 do decr k done;

  let h = !k / 2 in

  for i = 0 to h do
    typ.(fst s.(i)) <- Near;
    bitno.(fst s.(i)) <- i
  done;

  for i = h + 1 to !k do
    typ.(fst s.(i)) <- Far;
    bitno.(fst s.(i)) <- i - h - 1
  done;

  let p = !k - h in

  (* Dynamic-program far->near transitions *)

  let lpred = Array.make (n + 1) [] in
  let rpred = Array.make (n + 1) [] in
  for i = 1 to n - 1 do
    if typ.(i) = Far then
      let l, r = g.(i) in
      lpred.(l) <- i :: lpred.(l);
      rpred.(r) <- i :: rpred.(r)
  done;

  let dim = 1 lsl p in
  let reach = Array.make_matrix p dim 0 in
  let steps = Array.make_matrix p dim 0 in
  let state = Array.make_matrix p dim 0 in

  let near = ref 0 in

  let rec bckprop = function
  | [] -> ()
  | (edg, pos, stp, msk, pat, xxx) :: more ->
    let b = bitno.(pos) in
    let z = 1 lsl b in
    let valid =
      msk land z = 0 ||
        match edg with
        | L -> pat land z = z
        | R -> pat land z = 0
    in
    let msk = msk lor z in
    let pat = match edg with L -> pat land (lnot z) | R -> pat lor z in
    let xxx = xxx lxor z in
    if not valid then bckprop more else
      begin
      let s = ref 0 in
      while !s < dim do
        assert (reach.(b).(!s + pat) = 0);
        reach.(b).(!s + pat) <- !near;
        steps.(b).(!s + pat) <- stp;
        state.(b).(!s + pat) <- (!s + pat) lxor xxx;
        s := (!s + 1 + msk) land (lnot msk)
      done;

      let l = List.map (fun p -> L, p, (1 + stp), msk, pat, xxx) lpred.(pos) in
      let r = List.map (fun p -> R, p, (1 + stp), msk, pat, xxx) rpred.(pos) in
      bckprop (l @ r @ more)
      end
  in

  for i = 1 to n do
    if typ.(i) = Near then begin
      near := i;
      List.iter (fun p -> bckprop [ L, p, 1, 0, 0, 0 ]) lpred.(i);
      List.iter (fun p -> bckprop [ R, p, 1, 0, 0, 0 ]) rpred.(i)
    end
  done;

  (*
  for i = 1 to n do
    if typ.(i) = Far then begin
      printf "--\n%d:\n" i;
      let b = bitno.(i) in
      Array.iteri (fun s near ->
        printf "%8x: (%d,%x) %d\n" s near state.(b).(s) steps.(b).(s)) reach.(b)
    end
  done;
  *)

  (* Now simulate in big steps *)

  let s1 = ref 0 in
  let s2 = ref 0 in
  let limit = (h + 1) * (1 lsl (h + 1)) in
  let pos = ref 1 in
  let t = ref 0 in
  try
    let rec loop limit =
      if !pos = n then raise (Exit !t) else
      match typ.(!pos) with
      | Dead -> ()
      | Far ->
          let b = bitno.(!pos) in
          t := !t + steps.(b).(!s1);
          pos := reach.(b).(!s1);
          s1 := state.(b).(!s1);
          if !pos <> 0 then loop limit
      | Near ->
        if limit > 0 then begin
          let bit = 1 lsl bitno.(!pos) in
          let s' = !s2 lxor bit in
          pos := if !s2 land bit = 0 then fst g.(!pos) else snd g.(!pos);
          s2 := s';
          incr t;
          loop (limit - 1)
        end
    in
    loop limit;
    "Infinity"
  with
  | Exit i -> sprintf "%d" i

let solve (n, g) =
  if n <= 20 then brute (n, g) else split (n, g)

let solve_all fname =
  let ib = Scanning.from_file fname in
  let t = bscanf ib "%d\n" (fun t -> t) in
  for i = 1 to t do
    read_case ib |> solve |> (fun r -> printf "Case #%d: %s\n%!" i r)
  done

let () = if not !Sys.interactive then solve_all Sys.argv.(1)

