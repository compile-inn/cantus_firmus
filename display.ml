(* [show_note] converts the midi representation of a note to a traditional alphabetic representation *)
(* Ordre des bemols: Si, Mi, La, Ré, Sol, Do, Fa *)
let get_octave n =
  match n with
  | oct when oct > 0 && oct < 12 -> "-1"
  | oct when oct >= 12 && oct < 24 -> "0"
  | oct when oct >= 24 && oct < 36 -> "1"
  | oct when oct >= 36 && oct < 48 -> "2"
  | oct when oct >= 48 && oct < 60 -> "3"
  | oct when oct >= 60 && oct < 72 -> "4"
  | oct when oct >= 72 && oct < 84 -> "5"
  | oct when oct >= 84 && oct < 96 -> "6"
  | oct when oct >= 96 && oct < 108 -> "7"
  | oct when oct >= 108 && oct < 120 -> "8"
  | _ -> failwith "out of range"
  
let get_note n =
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

let show_note n =
  get_note n ^ get_octave n

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