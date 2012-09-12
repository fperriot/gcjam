open Printf
open Scanf

let ( |> ) x f = f x

let read_case cin =
    let n = fscanf cin "%d\n" (fun n -> n) in
    let rec loop n acc =
        if n = 0 then List.rev acc else
        loop (n - 1) (fscanf cin "%d " (fun x -> x) :: acc)
    in
    let xs = loop (n - 1) [] in
    n, xs

exception Impossible

type edge = Gt | Gte

let solve (n, xs) =
    let x = Array.of_list (0 :: xs) in
    let h = Array.make (n + 1) 0 in

    (* First gather all constraints on slopes *)

    let g = Hashtbl.create (n + n) in
    let r = Hashtbl.create (n + n) in
    let impose a gt b =
        Hashtbl.add g a (gt, b);
        Hashtbl.add r b (a, gt)
    in

    let rec hz i =
        if x.(i) <> n then
            impose (i, x.(i)) Gte (x.(i), x.(x.(i)));
        let rec loop p j =
            if j = x.(i) then x.(i) else
            if x.(j) > x.(i) then raise Impossible else
            if x.(j) < x.(i) then loop p (hz j) else
            let () = impose (j, x.(i)) Gt (p, x.(i)) in
            loop j (j + 1)
        in
        loop i (i + 1)
    in
    let rec gather i =
        if i = n then () else gather (hz i) in
    gather 1;

    (* Then assign slopes from lowest to highest in a topological traversal
       of the constraints DAG; arbitrarily pick 0 as the lowest slope *)

    let minima = Hashtbl.fold (fun v _ acc ->
        if Hashtbl.mem g v then acc else v :: acc) r [] in

    let slope = Hashtbl.create (n + n) in
    minima |> List.iter (fun v -> Hashtbl.add slope v 0);

    let relax (i, j) gt (p, q) =
    (*
        match gt with
        | Gt -> printf "Relaxing (%d, %d) > (%d, %d)\n" i j p q
        | Gte -> printf "Relaxing (%d, %d) >= (%d, %d)\n" i j p q
    *)
        let x = try Hashtbl.find slope (i, j) with Not_found -> 0 in
        let y = Hashtbl.find slope (p, q) in
        let z =
            match gt with
            | Gt -> y * (j - i) / (q - p) + 1
            | Gte -> (y * (j - i) + (q - p - 1)) / (q - p)
        in
        Hashtbl.replace slope (i, j) (max x z);
    in

    let rec assign m =
        if m = [] then () else
        m |> ListLabels.fold_left ~init:[] ~f:(fun acc v ->
            Hashtbl.find_all r v
            |> ListLabels.fold_left ~init:acc ~f:(fun acc (u, gt) ->
                relax u gt v;
                Hashtbl.remove r v;
                Hashtbl.remove g u;
                if Hashtbl.mem g u then acc else u :: acc))
        |> assign
    in
    assign minima;

    (* Next assign relative heights based on the slopes *)

    let g = Hashtbl.create (n + n) in
    slope |> Hashtbl.iter (fun (i, j) _ -> Hashtbl.add g i j;
                                           Hashtbl.add g j i);

    let vis = Hashtbl.create n in
    let rec dfs i x =
        if Hashtbl.mem vis i then () else
        let () = Hashtbl.add vis i (); h.(i) <- x in
        Hashtbl.find_all g i
        |> List.iter (fun j ->
            let s = Hashtbl.find slope (min i j, max i j) in
            dfs j (if i < j then h.(i) + s else h.(i) - s))
    in
    dfs 1 0;

    (* Finally normalize to non-negative heights *)

    let lowest = Array.fold_left min 0 h in
    Array.iteri (fun i x -> h.(i) <- x - lowest + 1) h;

    List.tl (Array.to_list h)

let solve_cases fname =
    let cin = open_in fname in
    let t = fscanf cin "%d\n" (fun t -> t) in
    for i = 1 to t do
        printf "Case #%d:" i;
        try
            let heights = solve (read_case cin) in
            List.iter (printf " %d") heights;
            printf "\n"
        with Impossible ->
            print_string " Impossible\n"
    done;
    close_in cin

let () = solve_cases Sys.argv.(1)

