let () = Random.self_init ()

type generator =
  { forward_cache: (string * string, (string, int) Hashtbl.t) Hashtbl.t
  ; backward_cache: (string * string, (string, int) Hashtbl.t) Hashtbl.t
  ; words: (int, string) Hashtbl.t
  ; last_two_words: string * string }

let list_random_el l = List.nth l (Random.int (List.length l))

let add_word generator w =
  let n = Hashtbl.length generator.words in
  Hashtbl.add generator.words n w ;
  {generator with last_two_words= (snd generator.last_two_words, w)}

let get_last_two_words generator =
  match generator.last_two_words with
  | "", "" ->
      []
  | x, "" | "", x ->
      [x]
  | x, y ->
      [x; y]

let init () =
  let gen =
    { forward_cache= Hashtbl.create 4096
    ; backward_cache= Hashtbl.create 4096
    ; words= Hashtbl.create 4096
    ; last_two_words= ("", "") }
  in
  add_word gen "\n"

let triples = function
  | w1 :: w2 :: s ->
      let _, _, acc =
        List.fold_left
          (fun (w1, w2, acc) el -> (w2, el, (w1, w2, el) :: acc))
          (w1, w2, []) s
      in
      List.rev acc
  | _ ->
      []

let rec add_key cache k v =
  match Hashtbl.find cache k with
  | exception Not_found ->
      Hashtbl.add cache k (Hashtbl.create 64) ;
      add_key cache k v
  | tbl -> (
    match Hashtbl.find tbl v with
    | exception Not_found ->
        Hashtbl.add tbl v 0
    | n ->
        Hashtbl.replace tbl v (n + 1) )

let feed generator msg =
  let splitted = String.split_on_char ' ' msg in
  let splitted = splitted @ ["\n"] in
  let triples = triples (get_last_two_words generator @ splitted) in
  List.iter
    (fun (w1, w2, w3) ->
      add_key generator.forward_cache (w1, w2) w3 ;
      add_key generator.backward_cache (w3, w2) w1)
    triples ;
  List.fold_left (fun generator el -> add_word generator el) generator splitted

let select_seed generator seed_word backward =
  let dir = if backward then -1 else 1 in
  match seed_word with
  | None ->
      let rec loop = function
        | "\n", _ | _, "\n" ->
            let seed =
              if backward then
                1 + Random.int (Hashtbl.length generator.words - 1)
              else Random.int (Hashtbl.length generator.words - 1)
            in
            loop
              ( Hashtbl.find generator.words seed
              , Hashtbl.find generator.words (seed + dir) )
        | seed_word, next_word ->
            (seed_word, next_word)
      in
      loop ("\n", "\n")
  | Some w ->
      let possible_indexes =
        Hashtbl.fold
          (fun k v acc -> if v = w then k :: acc else acc)
          generator.words []
      in
      let index = list_random_el possible_indexes + dir in
      (w, Hashtbl.find generator.words index)

let generate_markov_text generator max_size seed backward =
  let seed_word, next_word =
    match seed with
    | None, x | x, None ->
        select_seed generator x backward
    | Some x, Some y ->
        (x, y)
  in
  let cache =
    if backward then generator.backward_cache else generator.forward_cache
  in
  let w1, w2 =
    if Random.int 3 = 0 && Hashtbl.mem cache ("\n", seed_word) then
      ("\n", seed_word)
    else (seed_word, next_word)
  in
  let w1 = ref w1 in
  let w2 = ref w2 in
  let gen_words = ref [] in
  let exception Stop in
  ( try
      for _ = 0 to max_size do
        gen_words := !w1 :: !gen_words ;
        let tbl =
          try Hashtbl.find cache (!w1, !w2) with Not_found -> raise Stop
        in
        let cache_n = Hashtbl.fold (fun _ v acc -> acc + v) tbl 0 in
        let i = if cache_n = 0 then 0 else Random.int cache_n in
        let exception Found of string in
        let new_word =
          match
            Hashtbl.fold
              (fun k v acc ->
                let acc = acc + v in
                if i <= acc then raise (Found k) else acc)
              tbl 0
          with
          | exception Found s ->
              s
          | _ ->
              raise Stop
        in
        w1 := !w2 ;
        w2 := new_word
      done
    with Stop -> () ) ;
  if !w2 <> "\n" then gen_words := !w2 :: !gen_words ;
  let gen_words = List.filter (fun el -> el <> "\n") !gen_words in
  let buff = Buffer.create 512 in
  ( match if backward then gen_words else List.rev gen_words with
  | [] ->
      ()
  | x :: s ->
      Buffer.add_string buff x ;
      List.iter (fun el -> Buffer.add_string buff (" " ^ el)) s ) ;
  Buffer.contents buff
