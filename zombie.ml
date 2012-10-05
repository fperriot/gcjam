open Printf
open Scanf

let ( |> ) x f = f x

let read_case ib =
    let z = bscanf ib "%d\n" (fun z -> z) in
    let x = Array.make z 0 in
    let y = Array.make z 0 in
    let m = Array.make z 0 in
    for i = 0 to z - 1 do
        bscanf ib "%d %d %d\n" (fun xi yi mi ->
            x.(i) <- xi;
            y.(i) <- yi;
            m.(i) <- mi)
    done;
    z, x, y, m

let dist (x0, y0) (x1, y1) = max (abs (x0 - x1)) (abs (y0 - y1))

type state = { x: int; y:int; t:int; recharge_delay:int }

let print_state s =
    printf "x=%d y=%d t=%d d=%d" s.x s.y s.t s.recharge_delay

let time_of_kill (z, x, y, m) i s =
    let reach = s.t + 100 * dist (x.(i), y.(i)) (s.x, s.y) in
    let fire = s.t + s.recharge_delay in
    let vanish = m.(i) + 1000 in
    if reach <= vanish && fire <= vanish then
        max m.(i) (max reach fire)
    else
        -1

let pick targets parms s =
    let (z, x, y, m) = parms in
    let by_time = targets
        |> List.map (fun i -> i, time_of_kill parms i s)
        |> List.filter (fun (_, t) -> t <> -1)
        |> List.sort (fun (_, a) (_, b) -> compare a b)
    in
    let rec filter lst acc =
        match lst with
        | [] -> List.rev acc
        | (i, t) :: lst ->
            let s' = { x = x.(i); y = y.(i); t; recharge_delay = 750 } in
            let lst = List.filter (fun (j, t') ->
                                        let t'' = time_of_kill parms j s' in
                                        t'' > t' || t'' = -1) lst in
            filter lst ((i, t) :: acc)
    in
    filter by_time []

module S = Set.Make(struct type t = int;; let compare = compare end)

module C = struct
    type t = S.t * int * int
    let equal (s1, x1, y1) (s2, x2, y2) =
        x1 = x2 && y1 = y2 && S.equal s1 s2
    let hash = Hashtbl.hash
end

module H = Hashtbl.Make(C)

exception Max_score

let solve parms =
    let (z, x, y, m) = parms in
    let s0 = { x = 0; y = 0; t = 0; recharge_delay = 0 } in
    let all =
        let rec loop n acc = if n < 0 then acc else loop (n - 1)
                                                            (S.add n acc) in
        loop (z - 1) S.empty
    in
    let tmax = S.fold (fun i t -> max t (m.(i) + 1000)) all 0 in
    let body_count = ref 0 in
    let h = H.create 0 in
    let rec rampage k s targets =
        if !body_count = z then
            raise Max_score
        else
        if k + S.cardinal targets <= !body_count then
            () (*printf "Not enough targets remaining to beat record\n"*)
        else
        if tmax - s.t - s.recharge_delay <= 750 * (!body_count - k - 1) then
            () (*printf "Not enough shots remaining to beat record\n"*)
        else
        let seen_better =
            try
            let prev, prev_k = H.find h (targets, s.x, s.y) in
            prev.x = s.x &&
            prev.y = s.y &&
            prev.t <= s.t &&
            prev.recharge_delay <= s.recharge_delay &&
            prev_k >= k
            with Not_found -> false
        in
        if seen_better then () (*printf "*\n"*) else begin
        H.replace h (targets, s.x, s.y) (s, k);
        let primary = pick (S.elements targets) parms s in
        primary |> List.iter (fun (i, t) ->
                (*printf "Kill %d at %d\n" i t;*)
                body_count := max !body_count (k + 1);
                (*printf "Max body count %d/%d\n" !body_count z;*)
                let s' = { x = x.(i); y = y.(i); t; recharge_delay = 750 } in
                let tgts = targets |> S.remove i
                                   |> S.filter (fun j -> m.(j) + 1000 >= t) in
                rampage (k + 1) s' tgts)
        end
    in
    let () = try rampage 0 s0 all with Max_score -> () in
    !body_count

let solve_all fname =
    let ib = Scanning.from_file fname in
    let t = bscanf ib "%d\n" (fun t -> t) in
    for i = 1 to t do
        read_case ib |> solve |> (fun r -> printf "Case #%d: %d\n" i r)
    done

let () = if not !Sys.interactive then solve_all Sys.argv.(1)

