open Notty_unix

let () =
  let term = Term.create () in

  let rec loop xr yr zr t =
    let w, h = Term.size term in
    let image = Cube.render w h xr yr zr t in 
    Term.image term image;

    (* Unix.select waits for input on stdin. 
       Arguments: [read_fds], [write_fds], [except_fds], timeout *)
    match Unix.select [Unix.stdin] [] [] 0.1 with
    | ([], _, _) -> 
        (* Timeout occurred (no input): Update animation *)
        loop xr yr zr (t +. 0.1)
    | _ -> 
        (* Input is available: Handle the event *)
        match Term.event term with
        | `Key (`Escape, _) ->
            Term.release term;
            print_endline "Exiting..."
        | `Key (`Arrow `Up, _) ->
            loop (xr +. 0.1) yr zr (t +. 0.1)
        | `Key (`Arrow `Left, _) ->
            loop xr (yr +. 0.1) zr (t +. 0.1)
        | `Key (`Arrow `Right, _) ->
            loop xr yr (zr +. 0.1) (t +. 0.1)

        | _ -> 
            loop xr yr zr (t +. 0.1)
  in

  loop 0.0 0.0 0.0 0.0