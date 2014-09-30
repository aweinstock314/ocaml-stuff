(* This ocamlbuild plugin allows allows ocamlbuild to 
compile files starting with "#!/usr/bin/env ocaml", and 
automatically infer libraries to link from "#load" 
directives *)

open Ocamlbuild_plugin;;
let set_strip_preproc () = (
    Options.ocamldep := S[V"OCAMLDEP";A"-pp"; A"../remove_ocaml_toplevel_directives.sh"];
    Options.ocaml_cflags := ["-pp"; "../remove_ocaml_toplevel_directives.sh"] @ !Options.ocaml_cflags
);;

let strip_suffix s = String.sub s 0 (String.rindex s '.');;
let extract_loadname line =
    try
        (* regex isn't used since Str module can't be loaded for myocamlbuild.ml *)
        let quoted = Scanf.sscanf line "#load \"%s" (fun x -> x) in
        let unquoted = String.sub quoted 0 (String.index quoted '"') in
        Some(unquoted)
    with Scanf.Scan_failure _ -> None;;
let with_each_line_from_file ?bin fname f =
    with_input_file ?bin:bin fname (fun in_ch ->
        try while true do
        f (input_line in_ch)
        done with End_of_file -> ()
    );;
let add_libs_from_preprocs target =
    let add_lib libname = (
        Options.ocaml_libs @:= [strip_suffix x];
        Printf.printf "Adding \"%s\" to the libs list.\n%!" x
    ) in
    let add_file target = 
        let fname = (strip_suffix target) ^ ".ml" in
        Printf.printf "Source filename is \"%s\"\n%!" fname;
        with_each_line_from_file fname (fun line ->
            match extract_loadname line with
            | Some(x) -> add_lib x
            | None -> ()
        ) in
    add_file target;;

let strip_preproc_and_inject_libs = (function
| Before_options -> set_strip_preproc ()
| After_options -> List.iter add_libs_from_preprocs !Options.targets
| _ -> ()
);;

dispatch strip_preproc_and_inject_libs;;
