open Notty

type point2d = { u : float; v : float }

let xrot {Obj_loader.x; y; z} angle = 
  let c = cos angle in 
  let s = sin angle in
  { Obj_loader.x = x; 
               y = y *. c -. z *. s; 
               z = y *. s +. z *. c }

let yrot {Obj_loader.x; y; z} angle = 
  let c = cos angle in 
  let s = sin angle in
  { Obj_loader.x = x *. c +. z *. s; 
               y = y; 
               z = -.x *. s +. z *. c }

let zrot {Obj_loader.x; y; z} angle = 
  let c = cos angle in 
  let s = sin angle in
  { Obj_loader.x = x *. c -. y *. s; 
               y = x *. s +. y *. c; 
               z = z }

(* project 3D point to 2D using weak perspective projection *)
let project (p : Obj_loader.point3d) width height scale =
  let fov = 1.0 in
  let distance = 4.0 in
  let factor = fov /. (distance -. p.z) in
  (* center the projection *)
  let u = (p.x *. factor *. scale *. 2.0) +. (float_of_int width) /. 2.0 in
  let v = (p.y *. factor *. scale) +. (float_of_int height) /. 2.0 in
  { u; v }

let render_obj width height x_rot y_rot z_rot vertices edges =

  (* rot all vertices *)
  let rotated_verts = 
    List.map (fun v ->
    v |> (fun p -> xrot p x_rot)
      |> (fun p -> yrot p y_rot)
      |> (fun p -> zrot p z_rot)
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
