let generator words =
  let g = Omg.init () in
  let g = List.fold_left (fun gen el -> Omg.feed gen el) g words in
  g

let text gen max_length =
  Omg.generate_markov_text gen max_length (Some Constants.seed_word, Constants.next_word) false
