(* Google Code Jam 2012 Cruise Control problem.
 * We treat this as a graph-coloring problem.
 * Two graphs describe the state of the system at any given time point.
 * Cars are vertices of the graphs, and edges describe relationships between
 * pairs of cars.
 * 'crit' defines if two cars are in a critical section, i.e. one is
 * overtaking the other.
 * 'diff' defines if two cars necessarily belong to different lanes.
 * The first condition implies the second, so crit is a subgraph of diff.
 * But the second condition does not imply the first, as cars could be stuck
 * in different lanes due to previous events (car A just overtook car B, but
 * both are stuck in their lanes by two other cars.)
 * We maintain a coloring of the diff graph over time to verify that all cars
 * are in compatible positions. As soon as the diff graph is no longer
 * colorable, we report a collision.
 * We use colors 1, and 2 to denote Left and Right lanes, and color 0 to
 * express the special condition that a car could be in either lane.
 * Meaningful time points are when cars start or finish overtaking, or being
 * overtaken. We update the graphs at these times.
 * crit is easily maintained: add an edge when overtaking starts, remove it
 * when it finishes.
 * diff is harder: an edge is added when overtaking starts, similarly to crit,
 * but edges are only removed when the corresponding vertex in crit reaches
 * degree 0; i.e. when a car exiting a critical section with another one is
 * also free from any third-party.
 * The initial coloring of the diff graph is easy to perform: cars stuck in
 * lanes are assigned a color 1 or 2. Cars that are free to roam get color 0.
 * The maintenance of the coloring follows these rules:
 * Upon isolating a vertex (removing all edges leading to it) we set its color
 * to 0; i.e. a free car may roam in either lane.
 * Upon connecting two vertices: if both have definite colors, simply check
 * they are opposite. If only one has a definite color, we extend the coloring
 * on the other side. If both are undetermined (color 0), we verify that a
 * coloring exists, but don't pick one (in fact, it's enough to paint one
 * vertex with an arbitrary color, and verify that the coloring can be
 * extended to the whole connected component, because the c.c. is entirely
 * undetermined, so the opposite of any valid coloring is also valid.)
 * If we exit the last critical section with a valid coloring of the diff
 * graph, we've navigated through the entire problem.
 *)


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

let copy g =
    { sz = g.sz; adj = Array.init g.sz (fun a -> g.adj.(a)) }

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

exception Coloring_violation

let extend_coloring g v color clg =
    let rec bfs color clg = function
    | [] -> clg
    | vs ->
        let clg, adj = List.fold_left (fun (clg, adj) v ->
            match Coloring.find v clg with
            | 0 -> Coloring.add v color clg, G.adjacent g v @ adj
            | x ->
                if x = color then clg, adj else raise Coloring_violation
        ) (clg, []) vs in
        bfs (3 - color) clg adj
    in
    bfs color clg [v]

let rec drive crit diff clg = function
    | [] -> None
    | Leave (_, a, b) :: pts ->
        G.remove_edge crit a b;
        let clg = ref clg in
        if G.degree crit a = 0 then begin
            G.adjacent diff a |> List.iter (G.remove_edge diff a);
            clg := Coloring.add a 0 !clg
        end;
        if G.degree crit b = 0 then begin
            G.adjacent diff b |> List.iter (G.remove_edge diff b);
            clg := Coloring.add b 0 !clg
        end;
        drive crit diff !clg pts
    | Enter (t, a, b) :: pts ->
        G.add_edge crit a b;
        G.add_edge diff a b;
        match Coloring.find a clg, Coloring.find b clg with
        | 0, 0 ->
            let valid =
                try let _ = extend_coloring diff a 1 clg in true
                with Coloring_violation -> false
            in
            if valid then
                drive crit diff clg pts
            else
                Some (abs_float t)
        | 0, x ->
            let clg = extend_coloring diff a (3 - x) clg in
            drive crit diff clg pts
        | x, 0 ->
            let clg = extend_coloring diff b (3 - x) clg in
            drive crit diff clg pts
        | x, y ->
            if x = y then Some (abs_float t)
            else drive crit diff clg pts

let solve cars =
    let time_points = slice_time cars in
    let n = List.length cars in
    let crit = G.make n in
    let () = cars |> any2 (fun () a b ->
        if dist a b < 5.0 then
            G.add_edge crit a.id b.id) () in
    let diff = G.copy crit in
    let clg = cars |> ListLabels.fold_left ~init:Coloring.empty ~f:(
        fun clg car ->
            let color =
                if G.degree crit car.id = 0 then 0 else
                    match car.lane with
                    | 'L' -> 1
                    | 'R' -> 2
                    | _ -> failwith "bad input" in
            Coloring.add car.id color clg
    ) in
    drive crit diff clg time_points

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

