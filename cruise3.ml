let (|>) x f = f x

type car = { id: int; speed: float; pos: float; lane: char }

module G = struct

type t = { sz: int; adj: int list array }

let make n = { sz = n; adj = Array.make n [] }

let add_edge g a b =
    g.adj.(a) <- b :: g.adj.(a);
    g.adj.(b) <- a :: g.adj.(b)

let remove_edge g a b =
    g.adj.(a) <- List.filter (( <> ) b) g.adj.(a);
    g.adj.(b) <- List.filter (( <> ) a) g.adj.(b)

let degree g a = List.length g.adj.(a)

let adjacent g a = g.adj.(a)

end

let read_case cin =
    let n = Scanf.fscanf cin "%d\n" (fun n -> n) in
    let rec loop id acc =
        if id = n then List.rev acc else
        let car = Scanf.fscanf cin "%c %f %f\n"
            (fun lane speed pos -> { id; lane; speed; pos }) in
        loop (succ id) (car :: acc)
    in
    loop 0 []

let load_case fname =
    let cin = open_in fname in
    let cars = read_case cin in
    close_in cin;
    cars

let dist a b = abs_float (a.pos -. b.pos)

let rec any2 f x = function
    | []
    | [ _ ] -> x
    | a :: lst ->
        let y = List.fold_left (fun x b -> f x a b) x lst in
        any2 f y lst

let rec any3 f x = function
    | []
    | [ _ ] -> x
    | a :: lst ->
        let y = any2 (fun x b c -> f x a b c) x lst in
        any3 f y lst

type critical_section = Empty | Infinite | Interval of float * float

let rec critical a b =
    if a.speed = b.speed then
        if dist a b < 5.0 then Infinite else Empty
    else
        let x = (a.pos +. 5.0 -. b.pos) /. (b.speed -. a.speed) in
        let y = (a.pos -. 5.0 -. b.pos) /. (b.speed -. a.speed) in
        Interval (min x y, max x y)

type time_point = Enter of float * int * int
                | Leave of float * int * int

let cmp_time_points a b =
    match a, b with
    | Enter (x, _, _), Leave (y, _, _) when x = y -> +1
    | Leave (x, _, _), Enter (y, _, _) when x = y -> -1
    | Enter (x, _, _), Enter (y, _, _)
    | Leave (x, _, _), Leave (y, _, _)
    | Enter (x, _, _), Leave (y, _, _)
    | Leave (x, _, _), Enter (y, _, _) -> compare x y

let slice_time cars =
    cars
    |> any2 (fun points a b ->
        match critical a b with
        | Empty
        | Infinite -> points
        | Interval (x, y) ->
            if x >= 0. then Enter (x, a.id, b.id) ::
                            Leave (y, a.id, b.id) :: points else
            if y >= 0. then Leave (y, a.id, b.id) :: points else points
        ) []
    |> List.sort cmp_time_points

module Coloring = Map.Make(struct type t = int let compare = compare end)

let rec drive g h = function
    | [] -> None
    | Leave (_, a, b) :: pts ->
        G.remove_edge g a b;
        let da = G.degree g a in
        let db = G.degree g b in
        let h' = Hashtbl.create (Hashtbl.length h) in
        h |> Hashtbl.iter (fun c () ->
            let c = if da = 0 then Coloring.add a 0 c else c in
            let c = if db = 0 then Coloring.add b 0 c else c in
            Hashtbl.replace h' c ()
        );
        drive g h' pts
    | Enter (t, a, b) :: pts ->
        G.add_edge g a b;
        let h' = Hashtbl.create (Hashtbl.length h) in
        h |> Hashtbl.iter (fun c () ->
            match Coloring.find a c, Coloring.find b c with
            | 0, 0 ->
                let c1 = Coloring.add a 1 (Coloring.add b 2 c) in
                let c2 = Coloring.add b 1 (Coloring.add a 2 c) in
                Hashtbl.replace h' c1 ();
                Hashtbl.replace h' c2 ()
            | 0, x ->
                Hashtbl.replace h' (Coloring.add a (3 - x) c) ()
            | x, 0 ->
                Hashtbl.replace h' (Coloring.add b (3 - x) c) ()
            | x, y ->
                if x <> y then Hashtbl.replace h' c ()
        );
        if Hashtbl.length h' = 0 then Some (abs_float t) else drive g h' pts

let solve cars =
    let time_points = slice_time cars in
    let n = List.length cars in
    let g = G.make n in
    let () = cars |> any2 (fun () a b ->
        if dist a b < 5.0 then
            G.add_edge g a.id b.id) () in
    let c = cars |> ListLabels.fold_left ~init:Coloring.empty ~f:(
        fun c car ->
            let color =
                if G.degree g car.id = 0 then 0 else
                    match car.lane with
                    | 'L' -> 1
                    | 'R' -> 2
                    | _ -> failwith "bad input" in
            Coloring.add car.id color c
    ) in
    let h = Hashtbl.create 1 in
    Hashtbl.add h c ();
    drive g h time_points

let solve_cases fname =
    let cin = open_in fname in
    let t = Scanf.fscanf cin "%d\n" (fun t -> t) in
    for i = 1 to t do
        let cars = read_case cin in
        Printf.printf "Case #%d: " i;
        match solve cars with
        | None -> Printf.printf "Possible\n%!"
        | Some time -> Printf.printf "%f\n%!" time
    done;
    close_in cin

let () = solve_cases Sys.argv.(1)

