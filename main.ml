open Generator

let () = 
  Printf.printf "Select the cantus tone:";
  let tone = read_int () in

  Printf.printf "Select the cantus mode:";
  flush stdout;
  let mode = read_line () in

  Printf.printf "Select the cantus length in full notes:";
  flush stdout;
  let len = read_int () in

  let c = make_cantus_context len tone (get_mode mode) in
  cantufier c