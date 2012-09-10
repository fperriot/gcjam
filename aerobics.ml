open Printf
open Scanf

let ( |> ) x f = f x

let read_case cin =
    let n, w, l = fscanf cin "%d %f %f\n" (fun n w l -> n, w, l) in
    let rec loop n acc =
        if n = 0 then List.rev acc else
        loop (n - 1) (fscanf cin "%f " (fun r -> r) :: acc)
    in
    let rs = loop n [] in
    n, w, l, rs

let solve (n, w, l, rs) =
    Random.init 12345;
    let rec idx i acc = function
    | [] -> acc
    | r :: rs -> idx (i + 1) ((r, i) :: acc) rs
    in
    let rs = idx 0 [] rs in
    let rs = List.sort (fun (a, _) (b, _) -> compare b a) rs in
    let rec pin mat rs =
        match rs with
        | [] -> mat
        | (r, i) :: rs ->
            let isect (x, y, r, _) (x', y', r', _) =
                let dx = x -. x' in
                let dy = y -. y' in
                dx *. dx +. dy *. dy < (r +. r') *. (r +. r')
            in
            let rec coord () =
                let x = Random.float w in
                let y = Random.float l in
                if mat |> List.exists (isect (x,y,r,i)) then coord () else x, y
            in
            let x, y = coord () in
            pin ((x,y,r,i) :: mat) rs
    in
    pin [] rs
    |> List.sort (fun (_,_,_,i) (_,_,_,j) -> compare i j)

let solve_case cin =
    solve (read_case cin)

let solve_cases fname =
    let cin = open_in fname in
    let t = fscanf cin "%d\n" (fun t -> t) in
    for i = 1 to t do
        printf "Case #%d:" i;
        solve_case cin |> List.iter (fun (x, y, _, _) ->
            printf " %f %f" x y);
        printf "\n"
    done;
    close_in cin

let () = solve_cases Sys.argv.(1)

