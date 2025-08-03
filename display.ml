(* [show_note] converts the midi representation of a note to a traditional alphabetic representation *)
let show_note n = 
  match (n mod 12) with
  | 0 -> "C"
  | 1 -> "C#"
  | 2 -> "D"
  | 3 -> "D#"
  | 4 -> "E"
  | 5 -> "F"
  | 6 -> "F#"
  | 7 -> "G"
  | 8 -> "G#"
  | 9 -> "A"
  | 10 -> "A#"
  | 11 -> "B"
  | _ -> failwith "not in the scale"

let cantus_to_list cantus =
  Array.to_list cantus

  (* [show_scale] convers the midi representation of a scale into its readable representation *)
let show_scale s =
  List.map show_note s

let rec print_notes_h str lst =
  match lst with
  | [] -> str
  | h::[] -> h ^ "." ^ str
  | h::t -> h ^ ", " ^ print_notes_h str t 

let print_notes lst =
  let notes = print_notes_h "" lst in print_endline notes (* Adds a new line after printing. *)
  
(* [print_cantus] takes a cantus and print its readable representation. *)
let print_cantus cantus =
  let c = cantus_to_list cantus in
  print_notes (show_scale c)