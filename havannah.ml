open Printf
open Scanf

let ( |> ) x f = f x

let read_case ib =
    let s, m = bscanf ib "%d %d\n" (fun s m -> s, m) in
    let rec loop m acc =
        if m = 0 then List.rev acc else
        let i, j = bscanf ib "%d %d\n" (fun i j -> i, j) in
        loop (m - 1) ((i, j) :: acc)
    in
    s, m, loop m []

let on_board s (x, y) = x > 0 &&
                        y > 0 &&
                        x < s + s &&
                        y < s + s &&
                        y < x + s &&
                        x < y + s

let clockwise_around (x, y) =
    [ x, y + 1;
      x - 1, y;
      x - 1, y - 1;
      x, y - 1;
      x + 1, y;
      x + 1, y + 1 ]

let cell_flags s (x, y) =
    let z = s + s - 1 in
    if (x, y) = (1, 1) then 1 else
    if (x, y) = (1, s) then 2 else
    if (x, y) = (s, 1) then 4 else
    if (x, y) = (s, z) then 8 else
    if (x, y) = (z, s) then 16 else
    if (x, y) = (z, z) then 32 else
    if x = 1 then 64 else
    if y = 1 then 128 else
    if x = y + s - 1 then 256 else
    if y = x + s - 1 then 512 else
    if x = z then 1024 else
    if y = z then 2048 else 0

let corner_bits = 63
let edge_bits = 4095 - 63

let rec popcnt x = if x = 0 then 0 else 1 + popcnt (x land (x - 1))

let dups lst =
    let count = Hashtbl.create 0 in
    lst |> List.iter (fun e ->
        let c = try Hashtbl.find count e with Not_found -> 0 in
        Hashtbl.replace count e (c + 1));
    Hashtbl.fold (fun e c l -> if c > 1 then e :: l else l) count []

let dedup lst =
    let rec loop lst acc =
        match lst with
        | [] -> List.rev acc
        | [ x ] -> List.rev (x :: acc)
        | x :: y :: lst -> if x = y then loop (y :: lst) acc
                                    else loop (y :: lst) (x :: acc) 
    in
    loop lst []

let traverse board s xy f =
    let seen = Hashtbl.create 0 in
    let rec dfs = function
        | [] -> ()
        | (x, y) :: more ->
            if not (Hashtbl.mem seen (x, y)) then begin
                Hashtbl.add seen (x, y) ();
                f (x, y);
                let occupied_neighbors =
                    clockwise_around (x, y)
                    |> List.filter (fun (x, y) -> on_board s (x, y) &&
                                                  board.(x).(y) <> 0)
                in
                dfs (occupied_neighbors @ more)
                end
            else
                dfs more
    in
    dfs [ xy ]

let solve (s, m, moves) =
    let board = Array.make_matrix (s + s + 1) (s + s + 1) 0 in
    let cc_flags = Array.make (m + 1) 0 in
    let rec play moves i =
        match moves with
        | [] -> printf "none\n%!"
        | (x, y) :: moves ->
            let final = ref false in
            let flags = cell_flags s (x, y) in
            let around = clockwise_around (x, y) in
            let neighbors = List.filter (on_board s) around in
            let occupied = List.filter (fun (x, y) -> board.(x).(y) <> 0)
                                                                neighbors in
            let ccs = List.map (fun (x, y) -> board.(x).(y)) occupied in
            let f = ref flags in
            List.iter (fun j -> f := !f lor cc_flags.(j)) ccs;
            cc_flags.(i) <- !f;
            let connected_corners = !f land corner_bits in
            let connected_edges = !f land edge_bits in
            if popcnt connected_corners >= 2 then begin
                printf "bridge";
                final := true
            end;
            if popcnt connected_edges >= 3 then begin
                if !final then printf "-";
                printf "fork";
                final := true
            end;
            (* are we connecting two arms of some existing component? *)
            let biccs = dups ccs in
            (* for any such connection, is there an 'inside' and an 'outside' *)
            let rec ring = function
                | [] -> false
                | cc :: ccs ->
                    let is_cc = List.map (fun (x, y) -> board.(x).(y) = cc)
                                                                    around in
                    let buf = Array.of_list is_cc in
                    let flips = ref 0 in
                    Array.iteri (fun i x ->
                        if x <> buf.((i + 1) mod 6) then incr flips) buf;
                    !flips >= 4 || ring ccs
            in
            if ring biccs then begin
                if !final then printf "-";
                printf "ring";
                final := true
            end;
            (* connect the components *)
            traverse board s (x, y) (fun (x, y) -> board.(x).(y) <- i);
            if !final then
                printf " in move %d\n%!" i
            else
                play moves (i + 1)
    in
    play moves 1

let solve_all fname =
    let ib = Scanning.from_file fname in
    let t = bscanf ib "%d\n" (fun t -> t) in
    for i = 1 to t do
        printf "Case #%d: " i;
        read_case ib |> solve
    done

let () = if not !Sys.interactive then solve_all Sys.argv.(1)

