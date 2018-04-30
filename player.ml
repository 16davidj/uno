module DumbAI : Intelligence = struct
  type t = idk

  let rec does_wild4_exist hand =
    match hand with
    | [] -> None
    | h::t -> if h.effect = Wild4 then Some h else does_wild4_exist t

  let rec find_possible_card color num eff hand =
    match hand with
    | [] -> None
    | h::t -> if h.color = color then Some h else
      if h.value = num then Some h else
      if h.effect = eff then Some h else
      find_possible_card color num t

  let choose_card top_card hand =
    let exists_card = find_possible_card (top_card.color) (top_card.value) (top_card.effect) hand in
    match exists_card with
    | None -> begin
        match does_wild4_exist hand with
        | None -> Command.Draw
        | Some h -> Command.Play h
    end
    | Some h -> Command.Play h 
end
