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
let fold_file_lines ?bin fname f init =
    let acc = ref init in
    with_input_file ?bin:bin fname (fun in_ch ->
        try while true do
        acc := f (input_line in_ch) !acc
        done with End_of_file -> ()
    ); !acc;;
let add_libs_from_preprocs ?recursive target =
    let add_lib libname = (
        Options.ocaml_libs @:= [strip_suffix libname];
        Printf.printf "Adding \"%s\" to the libs list.\n%!" libname
    ) in
    let get_libs_of_file target = (
        let fname = (strip_suffix target) ^ ".ml" in
        Printf.printf "Source filename is \"%s\"\n%!" fname;
        try fold_file_lines fname (fun line acc ->
            match extract_loadname line with
            | Some(x) -> x :: acc
            | None -> acc
        ) [] with Sys_error _ -> []
    ) in
    let add_libs_of_file target = List.iter add_lib (get_libs_of_file target) in
    let module SS = Set.Make(String) in
    let rec recursive_add_libs targets finished_targets = (
        if SS.is_empty targets then () else begin
            let least = SS.min_elt targets in
            if SS.mem least finished_targets then () else begin
                let new_elts = get_libs_of_file least in
                List.iter add_lib new_elts;
                let targets1 = (List.fold_left (fun x y -> SS.add y x) targets new_elts) in
                let targets2 = (SS.remove least targets1) in
                recursive_add_libs targets2 (SS.add least finished_targets)
            end
        end
    ) in
    let recur = match recursive with | Some(x) -> x | None -> false in
    if not recur then add_libs_of_file target
    else recursive_add_libs (SS.add target SS.empty) SS.empty;;

let strip_preproc_and_inject_libs = (function
| Before_options -> set_strip_preproc ()
| After_options -> List.iter (add_libs_from_preprocs ~recursive:true) !Options.targets
| _ -> ()
);;

dispatch strip_preproc_and_inject_libs;;
