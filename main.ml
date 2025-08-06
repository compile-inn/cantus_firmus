open Generator

let () = 
  Printf.printf "Select the cantus tone (C, D, E, F, G, A, B):";
  let tone = read_line () in

  Printf.printf "Select the tone alteration (#, b, any key for none):";
  let alter = read_line () in

  Printf.printf "Select the tone height (range: -1 to 8):";
  let octave = read_line () in

  Printf.printf "Select the cantus mode (C, D, E, F, G, A, B):";
  flush stdout;
  let mode = read_line () in

  Printf.printf "Select the cantus length in full notes (range: 8 to 16):";
  flush stdout;
  let len = read_int () in

  let c = make_cantus_context len (get_tone tone alter octave) (get_mode mode) in
  cantufier c

  (* Add get_note to translate readable note into midi note:
  C4 -> 60
  Add support for flat alterations and scale: C4b *)