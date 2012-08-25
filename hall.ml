(* Google Jam 2012 Hall of mirrors problem
 *
 * The general idea is to follow a straight ray starting from the origin
 * - defined as the position of 'X' - along a gentle slope with an angle
 * between 0 and 45 degrees.
 * We fill up the remaining angles from 45 to 360 degrees by exploiting
 * symmetries.
 * When a mirror is hit along the ray, the world is flipped/rotated
 * according to the laws of reflection.
 * The world that the ray traverses is a virtual one, defined by an affine
 * transform of the original grid.
 * Whenever we need to lookup the contents of a square in the virtual world,
 * we translate its coordinates back to a position in the starting grid by
 * applying the inverse of the transform that the world went through.
 *)

type square = Empty | Self | Mirror

type transform = int * int * int * int * int * int

let compose (a, b, c, d, e, f) (a', b', c', d', e', f') =
    (*
        [ a b     [ a' b'
          c d ] *   c' d' ]

        [ a b     [ e'     [ e
          c d ] *   f' ] +   f ]
    *)
    a * a' + b * c',
    a * b' + b * d',
    c * a' + d * c',
    c * b' + d * d',
    a * e' + b * f' + e,
    c * e' + d * f' + f

let apply (a, b, c, d, e, f) (x, y) =
    a * x + b * y + e,
    c * x + d * y + f

let id = (1, 0, 0, 1, 0, 0)
let rrot = (0, 1, -1, 0, 0, 0)
let lrot = (0, -1, 1, 0, 0, 0)
let vflip = (-1, 0, 0, 1, 0, 0)
let hflip = (1, 0, 0, -1, 0, 0)
let xlat x y = (1, 0, 0, 1, x, y)

type grid = transform * square array array

let transform t (t', base) = compose t' t, base

let rrot = transform lrot
and lrot = transform rrot
let vflip x = transform (compose vflip (xlat (-x) 0))
let hflip y = transform (compose hflip (xlat 0 (-y)))
let origin x y = transform (xlat x y)

let rec square ((t, base):grid) x y =
    let x, y = apply t (x, y) in base.(y).(x)

let load_grid cin =
    let me_x = ref 0 in
    let me_y = ref 0 in
    let line = input_line cin in
    let h, w, d = Scanf.sscanf line "%u %u %u" (fun h w d -> h, w, d) in
    let base = Array.make_matrix h w Empty in
    for i = 1 to h do
        let line = input_line cin in
        for j = 0 to w - 1 do
            base.(h - i).(j) <-
                 match line.[j] with '#' -> Mirror
                                   | 'X' -> me_x := j; me_y := h - i; Self
                                   | '.' -> Empty
                                   | _ -> failwith "Bad grid"
        done
    done;
    (id, base), d, !me_x, !me_y

let load_grids fname =
    let cin = open_in fname in
    let n = Scanf.sscanf (input_line cin) "%u" (fun n -> n) in
    let rec loop n acc =
        if n = 0 then List.rev acc else
        loop (pred n) (load_grid cin :: acc)
    in
    let grids = loop n [] in
    close_in cin;
    grids

let details = ref false

let look slope grid d =
    let a = fst slope
    and b = snd slope in
    assert (a >= 0 && b >= 0);
    if !details then Printf.printf "look along %d/%d\n" a b;
    let rec step grid x y =
        if !details then Printf.printf "step %d %d\n" x y;
        if x * x + y * y > d * d then
            let () = if !details then Printf.printf "exceeding distance\n" in
            Empty
        else
        let move_right () =
            if !details then Printf.printf "moving right\n";
            if square grid (x + 1) y = Mirror then
                let () = if !details then Printf.printf "vflip\n" in
                step (vflip (x + x + 1) grid) (x + 1) y
            else
                step grid (x + 1) y
        in
        let move_up () =
            if !details then Printf.printf "moving up\n";
            if square grid x (y + 1) = Mirror then
                let () = if !details then Printf.printf "hflip\n" in
                step (hflip (y + y + 1) grid) x (y + 1)
            else
                step grid x (y + 1)
        in
        let locate_next_square () =
            (* (y + 1/2) / (x + 1/2) <=> b / a *)
            let delta = a * (y + y + 1) - b * (x + x + 1) in
            if delta > 0 then move_right () else
            if delta < 0 then move_up () else begin
            (* hit the corner *)
            if !details then Printf.printf "hit the corner\n";
            match square grid (x + 1) y,
                  square grid (x + 1) (y + 1),
                  square grid x (y + 1) with
            | Mirror, Mirror, Mirror ->
                if !details then Printf.printf "triple-mirror\n";
                let g = hflip (y + y + 1) (vflip (x + x + 1) grid) in
                step g (x + 1) (y + 1)
            | Mirror, Mirror, _ ->
                if !details then Printf.printf "double-mirror vertical\n";
                step (vflip (x + x + 1) grid) (x + 1) (y + 1)
            | _, Mirror, Mirror ->
                if !details then Printf.printf "double-mirror horizontal\n";
                step (hflip (y + y + 1) grid) (x + 1) (y + 1)
            | _, Mirror, _ ->
                if !details then Printf.printf "single absorbing mirror\n";
                Empty
            | _ ->
                if !details then Printf.printf "going through corner\n";
                step grid (x + 1) (y + 1)
            end
        in
        match square grid x y with
        | Self when x = 0 && y = 0 -> locate_next_square ()
        | Self when a * y = b * x -> Self
        | Self
        | Empty -> locate_next_square ()
        | Mirror -> failwith "Mirror"
    in
    step grid 0 0

let gcd a b =
    let rec gcd a b =
        if b = 0 then a else gcd b (a mod b)
    in
    if a > b then gcd a b else gcd b a

let scan_octant ~right ~diag grid d =
    let count = ref 0 in
    if right then
        if look (1, 0) grid d = Self then incr count;
    if diag then
        if look (1, 1) grid d = Self then incr count;
    for a = 1 to d do
        for b = 1 to a - 1 do
            if gcd a b = 1 then
            if look (a, b) grid d = Self then incr count
        done
    done;
    !count

let scan_quadrant grid d =
    scan_octant ~right:true ~diag:true grid d +
    scan_octant ~right:false ~diag:false (hflip 0 grid) d

let reflections grid d =
    scan_quadrant grid d +
    scan_quadrant (rrot grid) d +
    scan_quadrant (rrot (rrot grid)) d +
    scan_quadrant (lrot grid) d

let solve_case i base d ox oy =
    let r = reflections (origin ox oy base) d in
    Printf.printf "Case #%d: %d\n" i r

let solve_input fname =
    let cases = load_grids fname in
    let i = ref 1 in
    List.iter (fun (base, d, ox, oy) ->
        solve_case !i base d ox oy;
        incr i
    ) cases

let () = solve_input Sys.argv.(1)

