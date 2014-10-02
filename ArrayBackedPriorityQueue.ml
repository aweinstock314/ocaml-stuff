(* Priority queue implementation that uses an implicit heap structure *)

(* min-heap, with provided less-than relation "<%" *)
class ['a] priorityQueue filler (<%) =
let (parent_idx, lchild_idx, rchild_idx) = ((fun x -> (x-1)/2), (fun x -> 2*x+1), (fun x -> 2*x+2)) in
object(self)
(* positions from [0, !last_idx) contain valid data, 
backing is grown by doubling (for amortized cost reasons) *)
val mutable backing : 'a array = Array.create 1 filler
val last_idx = ref 0 (* currently used space (as opposed to capacity) *)
method size = !last_idx
method __contents = backing
method __internal_percolate_up idx =
    if idx = 0 then () else
    begin
        let pidx = parent_idx idx in
        let parent = backing.(pidx) in
        let current = backing.(idx) in
        if current <% parent then begin
            Array.swap_by_index backing pidx idx;
            self#__internal_percolate_up pidx
        end
    end
method __internal_percolate_down idx =
    let lidx, ridx = lchild_idx idx, rchild_idx idx in
    if lidx >= !last_idx then () else begin
        let smaller_child_idx = ref lidx in
        if (ridx < !last_idx) && (backing.(ridx) <% backing.(lidx)) then
            smaller_child_idx := ridx;
        if backing.(!smaller_child_idx) <% backing.(idx) then begin
            Array.swap_by_index backing !smaller_child_idx idx;
            self#__internal_percolate_down !smaller_child_idx
        end
    end
method __internal_push_back elem =
    Array.set backing !last_idx elem;
    self#__internal_percolate_up !last_idx;
    inplace ((+) 1) last_idx
method __internal_double_size =
    let padding = Array.create (Array.length backing) filler in
    backing <- Array.append backing padding
method __internal_halve_size =
    let first_half = Array.init ((Array.length backing)/2) (Array.get backing) in
    backing <- first_half
method add (elem : 'a) =
    if !last_idx = Array.length backing then self#__internal_double_size;
    self#__internal_push_back elem
method pop =
    Array.swap_by_index backing 0 (!last_idx - 1);
    last_idx := !last_idx - 1;
    self#__internal_percolate_down 0;
    if !last_idx < ((Array.length backing)/4) then self#__internal_halve_size;
    backing.(!last_idx)
method is_heap =
    (* debugging method (successfully pinpointed a bad indexing function) *)
    let rv = ref true in
    let failures = ref [] in
    let tester chld i elem =
        if ((chld i) < !last_idx) && (backing.(chld i) <% elem) then begin
            rv := false;
            List.push failures (i,chld i, elem)
        end
    in
    Array.iteri (fun i elem ->
        tester lchild_idx i elem;
        tester rchild_idx i elem
    ) backing;
    (!rv, !failures)
end;;

(* since the priority queue works well as a heapsort backend, 
that's a sign that it (almost definitely) works correctly *)
let heapsort_test size =
    let to_be_sorted = Array.init size (fun _ -> Random.int 1000) in
    let ground_truth = Array.copy to_be_sorted in Array.sort compare ground_truth;
    let pq = new array_priorityQueue 0 (<) in
    Array.iter pq#add to_be_sorted;
    let heapsorted_arr = Array.init size (fun _ -> pq#pop) in
    let equality_vector = Array.init size (fun i -> ground_truth.(i) = heapsorted_arr.(i)) in
    (ground_truth, heapsorted_arr, equality_vector, pq)
