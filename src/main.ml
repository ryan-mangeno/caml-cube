open Notty_unix

let get_obj_files dir =
  try
    Sys.readdir dir |> Array.to_list
    |> List.filter (fun f -> Filename.check_suffix f ".obj")
    |> List.map (fun f -> Filename.concat dir f)
    |> Array.of_list
  with _ -> [||]

let () =
  let term = Term.create () in
  let files = get_obj_files "objects" in
  
  if Array.length files = 0 then (
    Term.release term;
    Printf.printf "No .obj files found in objects/\n"; exit 1
  );

  let initial_model = Obj_loader.load_obj files.(0) in

  let rec loop xr yr zr idx (verts, edges) =
    let w, h = Term.size term in
    
    let image = Render.render_obj w h xr yr zr verts edges in 
    
    Term.image term image;

    match Unix.select [Unix.stdin] [] [] 0.05 with
    | ([], _, _) -> 
        loop xr yr zr idx (verts, edges)
    | _ -> 
        match Term.event term with
        | `Key (`Escape, _) -> Term.release term
        | `Key (`Arrow `Down, _) ->
            let next_idx = (idx + 1) mod (Array.length files) in
            let next_model = Obj_loader.load_obj files.(next_idx) in
            loop xr yr zr next_idx next_model
        | `Key (`Arrow `Up, _)    -> loop (xr +. 0.1) yr zr idx (verts, edges)
        | `Key (`Arrow `Left, _)  -> loop xr (yr +. 0.1) zr idx (verts, edges)
        | `Key (`Arrow `Right, _) -> loop xr yr (zr +. 0.1) idx (verts, edges)
        | _ -> loop xr yr zr idx (verts, edges)
  in

  loop 0.0 0.0 0.0 0 initial_model