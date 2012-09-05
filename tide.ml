open Printf
open Scanf

type case = { h: int; n: int; m: int; ceiling: int array array;
                                      floor: int array array }

let read_case cin =
    let h, n, m = fscanf cin "%d %d %d\n" (fun h n m -> h, n, m) in
    let ceiling = Array.make_matrix m n 0 in
    let floor = Array.make_matrix m n 0 in
    for i = 0 to n - 1 do
        for j = 0 to m - 1 do
            fscanf cin "%d " (fun x -> ceiling.(j).(i) <- x)
        done;
    done;
    for i = 0 to n - 1 do
        for j = 0 to m - 1 do
            fscanf cin "%d " (fun x -> floor.(j).(i) <- x)
        done;
    done;
(* transposed:
    let ceiling = Array.init n (fun _ ->
                  Array.init m (fun _ -> fscanf cin "%d " (fun x ->x))) in
    let floor   = Array.init n (fun _ ->
                  Array.init m (fun _ -> fscanf cin "%d " (fun x ->x))) in
*)
    { h; n; m; ceiling; floor }

type path = (int * int * int) list

module Path = struct
    type t = path
    let compare a b =
        match a, b with
        | [], [] -> 0
        | [], _ -> +1
        | _, [] -> -1
        | (ta, _, _) :: _, (tb, _, _) :: _ -> compare tb ta
end

module Frontier = Heap.Imperative(Path)

let rec explore case path frontier earliest =
    let h, n, m, ceiling, floor = case.h, case.n, case.m, case.ceiling,
                                                          case.floor in
    match path with
    | [] -> failwith "bad path"
    | (t, x, y) :: _ ->

    let next () =
        if Frontier.is_empty frontier then
            Hashtbl.find earliest (m - 1, n - 1)
        else
        let path = Frontier.pop_maximum frontier in
        explore case path frontier earliest
    in

    if Hashtbl.mem earliest (x, y) &&
       Hashtbl.find earliest (x, y) <= t then next () else
    let () = Hashtbl.replace earliest (x, y) t in

    let go x' y' =
        if x' < 0 || y' < 0 || x' > m - 1 || y' > n - 1 then () else
        if ceiling.(x').(y') - floor.(x').(y') < 50 then () else
        if ceiling.(x').(y') - floor.(x).(y) < 50 then () else
        if ceiling.(x).(y) - floor.(x').(y') < 50 then () else
        let water = h - t in
        let wait =
            let above_water = ceiling.(x').(y') - water in
            if above_water < 50 then
                50 - above_water (* time in tenths of a second *)
            else
                0
        in
        let water = water - wait in
        let cross =
            if t + wait = 0 then 0 else
            if water >= floor.(x).(y) + 20 then 10 else 100
        in
        let t' = t + wait + cross in
        Frontier.add frontier ((t', x', y') :: path)
    in
    go (x + 1) y;
    go (x - 1) y;
    go x (y + 1);
    go x (y - 1);
    next ()

let solve case =
    let t = explore case [0, 0, 0] (Frontier.create 1) (Hashtbl.create 0) in
    printf "%f\n" (float t /. 10.)

let solve_cases fname =
    let cin = open_in fname in
    let t = fscanf cin "%d\n" (fun t -> t) in
    for i = 1 to t do
        printf "Case #%d: " i;
        solve (read_case cin)
    done;
    close_in cin

let () = solve_cases Sys.argv.(1)

