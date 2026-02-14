type point3d = { x : float; y : float; z : float }

let load_obj filename =
  let ic = open_in filename in
  let white_space = Str.regexp "[ \t]+" in 

  let rec loop verts edges =
    try
      let line = String.trim (input_line ic) in 
      if line = "" || line.[0] = '#' then 
        loop verts edges
      else
        let parts = Str.split white_space line in
        match parts with
        | "v" :: x :: y :: z :: _ ->
            let v = { 
              x = float_of_string x; 
              y = float_of_string y; 
              z = float_of_string z 
            } in
            loop (v :: verts) edges

        | "f" :: indices ->
            let idxs = List.map (fun s -> 
              int_of_string (List.hd (String.split_on_char '/' s)) - 1
            ) indices in
            
            let rec face_to_edges acc = function
              | a :: b :: rest -> face_to_edges ((a, b) :: acc) (b :: rest)
              | [last] -> (last, List.hd idxs) :: acc
              | [] -> acc
            in
            loop verts (face_to_edges edges idxs)

        | _ -> loop verts edges
    with End_of_file ->
      close_in ic;
      (List.rev verts, edges)
  in
  loop [] []