open Generator

let note_from_str = function
  | "C" -> 0
  | "D" -> 2
  | "E" -> 4
  | "F" -> 5
  | "G" -> 7
  | "A" -> 9
  | "B" -> 11
  | _ -> failwith "Not a note"

let octave_from_str = function
  | "-1" -> 0
  | "0" -> 12
  | "1" -> 24
  | "2" -> 36
  | "3" -> 48
  | "4" -> 60
  | "5" -> 72
  | "6" -> 84
  | "7" -> 96
  | "8" -> 108
  | _ -> failwith "Octave out of range"

let alteration = function
  | "#" -> 1
  | "b" -> -1
  | _ -> 0

let get_tone note alter oct =
  let tone = note_from_str note + alteration alter + octave_from_str oct in
  tone

let get_mode = function (* add a function that remove caps from a string *)
  | "C" | "c" | "major" | "Major" -> major_scale
  | "D" | "d" -> d_mode
  | "E" | "e" -> e_mode
  | "F" | "f" -> f_mode
  | "G" | "g" -> g_mode
  | "A" | "a" -> a_mode
  | _ -> major_scale (* fix this by adding a mode type *)