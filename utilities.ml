(* Generic helper functions, contains overrides/shadowings 
of some built-in modules to place extended functionality 
where it best fits *)

#load "str.cma";;
#load "unix.cma";;

let inplace fn xr = xr := fn !xr;;
let inrange lo hi x = (lo <= x) && (x < hi);;
let inrange2d lo hi (x,y) = let p = inrange lo hi in p x && p y;;
module List =
struct
include List;;
let split_at_pos n lst =
    let (r_pre, r_post, _) = (fold_left
        (fun (acc_pre, acc_post, i) elem ->
            if i < n
            then (elem::acc_pre, acc_post, i+1)
            else (acc_pre, elem::acc_post, i+1)
        ) ([], [], 0) lst)
    in (rev r_pre, rev r_post);;
let random_element lst = let idx = Random.int (length lst) in nth lst idx;;
let flatmap f lst = fold_left append [] (map f lst);;
let cons x y = x::y;;
let push lr elem = inplace (cons elem) lr;;
end;;
module Array =
struct
include Array
let swap_by_index arr i1 i2 =
    let tmp = arr.(i1) in
    arr.(i1) <- arr.(i2);
    arr.(i2) <- tmp;;
let swap2d (x1, y1) (x2, y2) arr =
    let tmp = arr.(y1).(x1) in
    arr.(y1).(x1) <- arr.(y2).(x2);
    arr.(y2).(x2) <- tmp;;
let init2d w h f = Array.init h (fun y -> Array.init w (fun x -> f x y));;
let iter2d f arr =
    Array.iteri (fun y row ->
        Array.iteri (fun x elem ->
            f x y elem
        ) row
    ) arr;;
let fold2di f init arr =
    let acc = ref init in
    iter2d (fun x y e -> inplace (f x y e) acc) arr;
    !acc;;
let map2di f arr =
    Array.init (Array.length arr) (fun y ->
        Array.init (Array.length arr.(y)) (fun x ->
            f x y arr.(y).(x)));;
let fold2d f = fold2di (fun x y -> f);;
let map2d f = map2di (fun x y -> f);;
let copy2d = map2d (fun x -> x);;
let findidx2d pred arr =
    match fold2di (fun x y e a -> match a with
        | Some(_) -> a
        | None -> if pred e then Some((x,y)) else None
    ) None arr with | Some(t) -> t | None -> raise Not_found;;
end;;
module Hashtbl =
struct
include Hashtbl
let of_list lst =
    let tbl = Hashtbl.create 0 in
    List.iter (fun (k,v) -> Hashtbl.add tbl k v) lst;
    tbl;;
end;;
let inchannel_of_string s =
    (* implementation of idea outlined in http://stackoverflow.com/a/20576176 *)
    let (reader, writer) = Unix.pipe () in
    Printf.fprintf (Unix.out_channel_of_descr writer) "%s%!" s;
    Unix.close writer;
    Unix.in_channel_of_descr reader;;
