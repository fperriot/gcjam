open Printf
open Scanf

let ( |> ) x f = f x

let read_ints ib n =
    let rec loop acc n =
        if n = 0 then List.rev acc else
        let i = bscanf ib " %d" (fun i -> i) in
        loop (i :: acc) (n - 1)
    in
    loop [] n

let eol ib = bscanf ib "\n" ()

let read_case ib =
    let n = bscanf ib "%d\n" (fun n -> n) in
    let t = Array.of_list (read_ints ib n) in
    eol ib;
    let p = Array.of_list (read_ints ib n) in
    eol ib;
    n, t, p

let zero_to n =
    let rec loop n acc =
        let acc = n :: acc in
        if n = 0 then acc else loop (n - 1) acc
    in
    loop n []

let solve (n, t, p) =
    zero_to (n - 1)
    |> List.sort (fun i j -> compare (t.(i) * p.(j), i) (t.(j) * p.(i), j))

let solve_all fname =
    let ib = Scanning.from_file fname in
    let t = bscanf ib "%d\n" (fun t -> t) in
    for i = 1 to t do
        printf "Case #%d:" i;
        read_case ib |> solve |> List.iter (printf " %d");
        printf "\n";
    done

let () = if not !Sys.interactive then solve_all Sys.argv.(1)

