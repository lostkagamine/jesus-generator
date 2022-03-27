let open_bible =
  let ic = open_in "bible.txt" in
  try
    let total_len = in_channel_length ic
  in
    let d = really_input_string ic total_len in d
  with e ->
    close_in_noerr ic;
    print_endline "OH FUCK";
    flush stdout;
    raise e

let should_keep c =
  match c with
  | '\n' -> Some ' '
  | '\t' -> None
  | '\r' -> None
  | _ -> Some c

let filter_bible b =
  let seq = String.to_seq b in
  Seq.filter_map should_keep seq

let passage_re =
  Str.regexp "\\([0-9]+\\):\\([0-9]+\\)"

let remove_passages str =
  Str.global_replace passage_re "" str

let consec_ws_re =
  Str.regexp " \\( +\\)"

let filter_consec_whitespace str =
  Str.global_replace consec_ws_re " " str

let back_to_str s =
  String.of_seq s

let write_output s =
  let oc = open_out "output.txt" in
  try
    output_string oc s
  with e ->
    close_out_noerr oc;
    print_endline "shit";
    flush stdout;
    raise e

let process_bible_full =
  open_bible |>
  remove_passages |>
  filter_bible |>
  back_to_str |>
  filter_consec_whitespace

let () =
  let bible =
    process_bible_full |>
    String.split_on_char ' '
  in
  let gen =
    (
      print_endline "initialising generator"
    );
    Markov.generator bible
  in
  (
    print_endline "generating phrases"
  );
  List.init 10 succ |>
  List.map (fun x -> begin
    print_endline ("generating phrase number: " ^ (string_of_int x));
    Markov.text gen Constants.max_length
  end) |>
  List.map (fun x -> begin
    print_endline x;
  end; x) |>
  String.concat "\n\n" |>
  write_output
