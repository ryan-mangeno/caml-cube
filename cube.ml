(* cube.ml *)
open Notty

type point3d = { x : float; y : float; z : float }
type point2d = { u : float; v : float }

(* define the 8 vertices of a cube centered at (0,0,0) *)
let vertices = [
  { x = -1.; y = -1.; z = -1. };
  { x = 1.; y = -1.; z = -1. };
  { x = 1.; y = 1.; z = -1. };
  { x = -1.; y = 1.; z = -1. };
  { x = -1.; y = -1.; z = 1. };
  { x = 1.; y = -1.; z = 1. };
  { x = 1.; y = 1.; z = 1. };
  { x = -1.; y = 1.; z = 1. };
]

(* define edges by connecting vertex indices *)
let edges = [
  (0, 1); (1, 2); (2, 3); (3, 0); (* Back face *)
  (4, 5); (5, 6); (6, 7); (7, 4); (* Front face *)
  (0, 4); (1, 5); (2, 6); (3, 7); (* Connecting edges *)
]

let xrot {x; y; z} angle = 
  let c = cos angle in 
  let s = sin angle in
  { x = x; 
    y = y *. c -. z *. s; 
    z = y *. s +. z *. c }

let yrot {x; y; z} angle = 
  let c = cos angle in 
  let s = sin angle in
  { x = x *. c +. z *. s; 
    y = y; 
    z = -.x *. s +. z *. c }

let zrot {x; y; z} angle = 
  let c = cos angle in 
  let s = sin angle in
  { x = x *. c -. y *. s; 
    y = x *. s +. y *. c; 
    z = z }

(* project 3D point to 2D using weak perspective projection *)
let project (p : point3d) width height scale =
  let fov = 1.0 in
  let distance = 4.0 in
  let factor = fov /. (distance -. p.z) in
  (* Center the projection *)
  let u = (p.x *. factor *. scale *. 2.0) +. (float_of_int width) /. 2.0 in
  let v = (p.y *. factor *. scale) +. (float_of_int height) /. 2.0 in
  { u; v }

(* render cube *)
let render width height x_rot y_rot z_rot dt =

  let px = cos dt in
  let py = sin dt in

  (* Rotate all vertices *)
  let rotated_verts = 
    List.map (fun v ->
      let p = v |> (fun p -> xrot p x_rot)
                |> (fun p -> yrot p y_rot)
                |> (fun p -> zrot p z_rot) in
      { x = p.x +. px ;
        y = p.y +. py ; 
        z = p.z }
    ) vertices in

  (* project all verts to 2d *)
  let projected_verts = Array.of_list (List.map (fun v -> project v width height 25.0) rotated_verts) in
  
  (* rasterize lines between projected points *)
  let points = 
    List.fold_left (fun acc (start_idx, end_idx) ->
      let p1 = projected_verts.(start_idx) in
      let p2 = projected_verts.(end_idx) in
      
      let steps = max (abs_float (p2.u -. p1.u)) (abs_float (p2.v -. p1.v)) |> int_of_float in
      let steps = max 1 steps in
      
      let dx = (p2.u -. p1.u) /. (float_of_int steps) in
      let dy = (p2.v -. p1.v) /. (float_of_int steps) in
      
      let rec interpolate i acc_pts =
        if i > steps then acc_pts
        else
          let x = int_of_float (p1.u +. dx *. (float_of_int i)) in
          let y = int_of_float (p1.v +. dy *. (float_of_int i)) in
          let new_acc = 
            if x >= 0 && x < width && y >= 0 && y < height then
              (x, y) :: acc_pts
            else
              acc_pts
          in
          interpolate (i + 1) new_acc 
      in
      interpolate 0 acc
    ) [] edges
  in
  
  (* convert list of coordinates to an image *)
  I.tabulate width height (fun x y ->
    if List.mem (x, y) points then
      I.string A.(fg lightgreen ++ st bold) "*"
    else
      I.void 1 1
  )
