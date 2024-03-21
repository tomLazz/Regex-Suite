open List
open Sets


type ('q, 's) transition = 'q * 's option * 'q

type ('q, 's) nfa_t = {
  sigma: 's list;
  qs: 'q list;
  q0: 'q;
  fs: 'q list;
  delta: ('q, 's) transition list;
}

let explode (s: string) : char list =
  let rec exp i l =
    if i < 0 then l else exp (i - 1) (s.[i] :: l)
  in
  exp (String.length s - 1) []



let move (nfa: ('q,'s) nfa_t) (qs: 'q list) (s: 's option) : 'q list = 
  let rec state_aux state_list = match state_list with
    | [] -> []
    | h::t -> union (List.fold_left (fun accum translation -> match translation with
      (s1, trans, s2) -> if h = s1 && s = trans then insert s2 accum else accum) [] nfa.delta) (state_aux t)
  in state_aux qs 
    
let e_closure (nfa: ('q,'s) nfa_t) (qs: 'q list) : 'q list =
  let rec e_aux state_list = match state_list with
    |[] -> []
    |lst -> let accum = (union (move nfa lst None) (lst)) in if accum = union (move nfa accum None) (accum) then accum else e_aux (union (move nfa accum None) (accum))
  in e_aux qs 


let accept (nfa: ('q, char) nfa_t) (s: string) : bool =
  let rec aux_acpt states letters = match letters with
    | [] -> states
    | h::t -> let next_states = e_closure nfa (move nfa states (Some h)) in aux_acpt next_states t in 
  let final_states = aux_acpt (e_closure nfa [nfa.q0]) (explode s) in 
  not (List.filter (fun state -> List.mem state nfa.fs) final_states = [])


(**)

let new_states (nfa: ('q,'s) nfa_t) (qs: 'q list) : 'q list list = 
  let rec nstate_aux list = match list with
    | [] -> []
    | h::t -> union ([e_closure nfa (move nfa qs (Some h))]) (nstate_aux t)
  in nstate_aux nfa.sigma

let new_trans (nfa: ('q,'s) nfa_t) (qs: 'q list) : ('q list, 's) transition list =
  let rec trans_aux list = match list with
    |[] -> []
    |h::t -> let next_states = e_closure nfa (move nfa qs (Some h)) in (qs, Some h, next_states) :: trans_aux t
  in trans_aux nfa.sigma

let new_finals (nfa: ('q,'s) nfa_t) (qs: 'q list) : 'q list list = 
  let rec finals_aux list = match list with
    |[] -> []
    |h::t -> if subset [h] nfa.fs then [qs] else finals_aux t
  in finals_aux qs


let rec remove_empty lst= match lst with
  |[] -> []
  |(_, _, []) :: t -> remove_empty t
  | h::t -> h:: remove_empty t

let rec in_states states value =
    match states with
    |[] -> false
    |h::t -> if eq h value then true else in_states t value
    
let rec nfa_to_dfa_step nfa dfa work visited =
  match work with 
  |[] -> dfa
  |[] :: t -> nfa_to_dfa_step nfa dfa t visited
  | h::t -> if in_states visited h
      then 
        (nfa_to_dfa_step nfa dfa t visited) 
      else 
        let qs_new = remove [] (List.sort_uniq Stdlib.compare (new_states nfa h)) in
        let delta_new = remove_empty (List.sort_uniq Stdlib.compare (new_trans nfa h)) in
        let fs_new = List.sort_uniq Stdlib.compare (new_finals nfa h) in
        nfa_to_dfa_step nfa {sigma = dfa.sigma; qs = union (qs_new) (dfa.qs); q0 = dfa.q0; fs = union (fs_new) (dfa.fs); delta = union (delta_new) (dfa.delta)}
          (union qs_new t) (union [h] visited)
  

let nfa_to_dfa (nfa: ('q,'s) nfa_t) : ('q list, 's) nfa_t =
  let work = e_closure nfa [nfa.q0] in 
  let init_dfa = {sigma = nfa.sigma; qs = []; q0 = work; fs = []; delta = []} in
  let almost = nfa_to_dfa_step nfa init_dfa [work] [] in if in_states almost.qs almost.q0 then almost else
    {sigma = almost.sigma; qs = insert almost.q0 almost.qs; q0 = almost.q0; fs = almost.fs; delta = almost.delta}
    
  
