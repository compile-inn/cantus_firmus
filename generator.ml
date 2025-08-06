open Display

(* Rule: Le cantus firmus est avant tout un chant de la voix. Ainsi, la simplicité est de mise.
Les petits intervalles sont plus simple à chanter que les grands. *)

type cantus_context = {
  cantus: int array; (* make it a (degree, note, interval) list? *)
  len: int;
  domain: int array;
  shape: int array;
  redirected: int list;
}

(* Available modes *)
let major_scale = [2; 2; 1; 2; 2; 2; 1]
let d_mode = [2; 1; 2; 2; 2; 1; 2]
let e_mode = [1; 2; 2; 2; 1; 2; 2]
let f_mode = [2; 2; 2; 1; 2; 2; 1]
let g_mode = [2; 2; 1; 2; 2; 1; 2]
let a_mode = [2; 1; 2; 2; 1; 2; 2] (* mode mineur naturel *)


(* [make_scale_h] takes a list containing the tone and a list containing the mode contents *)
let rec make_scale_h acc mode = 
  match mode with
  | [] -> acc
  | h::tl -> 
    match acc with 
      | [] -> failwith "acc empty" 
      | hacc::_ -> make_scale_h (hacc + h :: acc) tl

(* [make_scale] cons the tone into the acc and reverses the result of the helper function *)
let make_scale note mode =
  let acc = note::[] in let scale = List.rev (make_scale_h acc mode) in scale

(* creates an ordered set of notes accessible by their degree index *)
let indexed_scale lst =
  Array.of_list lst

(* Domain from which the cantus notes will be selected. 
  [make_cantus_domain] takes a [ton] and a [mode] and returns the scale in the array form.
  [let m = make_cantus_domain 60 major_scale] returns val m : int array = [|60; 62; 64; 65; 67; 69; 71; 72|]
 *)
let make_cantus_domain ton mode =
  let scale = make_scale ton mode in
  indexed_scale scale

(* Array from which the cantus is created note by note. 
   [init_cantus] take a cantus length and a tone. It returns an array of length between 8 and 16 tone.
   [init_cantus 8 60] returns [[|60; 60; 60; 60; 60; 60; 60; 60|]]*)
let init_cantus len ton = 
  let cantus = Array.make len ton in
  (cantus)
let make_cantus_context len ton mode =
  let cantus = init_cantus len ton in
  let domain = make_cantus_domain ton mode in
  let shape = Array.make len 0 in (* use to be len - 1*)
  let context = {
    cantus = cantus;
    len = len;
    domain = domain;
    shape = shape;
    redirected = [0]
  } in context

(* This randomly chooses a note from the cantus domain.
   [generate_note] takes a [scale] array and returns a random note from that parameter *)
let generate_note domain = 
  Random.self_init ();
  let domain_degree = Random.int_in_range ~min:0 ~max:7 in
  (domain_degree, domain.(domain_degree))

  (* [first_note] takes a cantus context and returns it with a randomly chosen tonic or its octave inserted. 
Rule: Cantus firmus always starts by the tonic of the tonality. *)
let first_note cantus_context =
  Random.self_init ();
  let cc = cantus_context in
  let tonic = 0 in
  let first = Random.bool () in 
  if first then cc.cantus.(tonic) <- cc.domain.(0) else cc.cantus.(tonic) <- cc.domain.(0) + 12;
  Printf.printf "Tonic is: %d\n" cc.cantus.(tonic);
  cc

(* [last_note] takes a cantus array and returns it updated with a randomly chosen last note. 
Rule: Cantus firmus always end on the tonic of the tonality *)
let last_note cantus_context = 
  Random.self_init ();
  let cc = cantus_context in
  let final = cc.len - 1 in
  let last = Random.bool () in
  if last then cc.cantus.(final) <- cc.cantus.(0) else cc.cantus.(final) <- cc.domain.(0) + 12; 
  Printf.printf "Final note is: %d\n" cc.cantus.(final);
  cc

(* [second_to_last] takes a scale and returns the possible second to last note in the cantus 
Rule: Il est généralement mieux d’arriver à la tonique finale par le haut (en descendant donc). *)
let second_to_last cantus_context =
  let cc = cantus_context in
  let end_note = cc.len - 1 in
  let penultimate = cc.len - 2 in
  let tonique = cc.domain.(0) in
  let second = cc.domain.(1) in
  let seventh = cc.domain.(6) in
  if cc.cantus.(end_note) = tonique then cc.cantus.(penultimate) <- second
  else cc.cantus.(penultimate) <- seventh;
  (* Printf.printf "Penultimate note is: %d\n" cc.cantus.(penultimate); *)
  cc

(* [move_check n1 n2] checks the interval between [n1] and [n2] 
    where [n1] is a note in the cantus and [n2] a potential successor.
    The function returns [Some n2] if the interval is consonnant, [None] if not. *)
let move_check current_note next_note =
  let interval = current_note - next_note in
    match interval with
    | 0 | 3 | 4 | 5 | 7 | 8 | 9 | 12 
    | -3 | -4 | -5 | -7 | -8 | -9 | -12 -> Some (interval, next_note) 
    | _ -> None

(* [redirect] redirects the melody in the opposite direction 
by moving one degree away from the leap note.*)
let redirect interval domain domain_degree =
  if interval < 0 then domain.(domain_degree - 1) else domain.(domain_degree + 1) 

let increase_cursor = function
  | h :: t -> h + 1 :: h :: t
  | [] -> 1 :: []
  

(* [record_shape] records the interval between a potential note and its precedent in the cantus. 
    The base case is first note - first note -> 0 *)
let record_shape arr cursor n1 n2 =
  let interval = n1 - n2 in
  arr.(cursor) <- interval

(* Helper function for [body_notes]. make cursor for domain and for cantus. make function for cursor update. *)
let rec body_notes_h cantus_context (counters: int * int list * int list * int) : cantus_context =
  let cc = cantus_context in
  let (acc, c_cursor, sh_cursor, redir) = counters in
  let body_end_note = cc.len - 3 in

  if acc = body_end_note then cc
  else if redir <> 0 then (cc.cantus.(List.hd c_cursor) <- redir; Printf.printf "Redirected note: %d\n" redir;
    body_notes_h cc (acc + 1, (increase_cursor c_cursor), (increase_cursor sh_cursor), 0) 
  ) else (
      let (domain_degree, next_note) = generate_note cc.domain in
      let current_note = cc.cantus.(List.hd c_cursor -1) in (* Do we need another note? Current_note always compare to 60 *)
      let result = if current_note = 0
        then move_check cc.cantus.(0) next_note    
        else move_check current_note next_note in
      
        match result with 
      | None -> body_notes_h cc counters
      | Some (interval, next_note) -> 

          (* Printf.printf "C Cursor is: %d, Current Note: %d, Next Note: %d Interval: %d Domain Degree: %d\n" (List.hd c_cursor) current_note next_note interval domain_degree; *)
          (* comparison between current note and next in wrong as current note is always 60 - the not yet modified one. *)
          let redirected_note =
          if abs interval > 5 then 
            redirect interval cc.domain domain_degree
          else 0 in 

          cc.cantus.(List.hd c_cursor) <- next_note; cc.shape.(List.hd sh_cursor) <- interval; 
          if acc = body_end_note then cc (* NEEDED here? *)
          else body_notes_h cc (acc + 1, (increase_cursor c_cursor), (increase_cursor sh_cursor), redirected_note)
      )

(* [body_notes] takes a cantus_context and generates body notes for the cantus. *)
let body_notes cantus_context =
  body_notes_h cantus_context (0, [1], [0], 0) (* acc; c_cursor; sh_cursor, redirected note *)

let printer cantus_context =
  let c = cantus_context.cantus in
  print_cantus c

(* [cantufier] takes a cantus context and generates a complete cantus. *)
let cantufier cantus_context =
  cantus_context 
  |> first_note
  |> last_note
  |> second_to_last
  |> body_notes
  |> printer