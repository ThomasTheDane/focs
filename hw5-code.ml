(* FoCS Spring 2015

   Homework 5 code


   Name: Thomas Nattestad

   Email: thomas.nattestad@students.olin.edu

   Comments: Cool homework but I still long for a better way to debug, also curious why my type for accept_config is different from the one you got. I worked with Josh on the demon game. 

 *)



(* 
 * String <-> characters utility functions:
 *
 *   explode : string -> char list
 *      returns the list of characters making up a string
 *
 *   implode : char list -> string
 *      concatenates the list of characters into a string
 *
 *)

let explode (str) = 
  let rec acc (index,result) = 
    if (index<0) then
      result
    else
      acc(index-1, (String.get str index)::result)
  in
    acc(String.length(str)-1, [])

let implode (cs) = 
  let str = String.create(List.length(cs)) in
  let rec loop (cs,index) = 
    match cs with
      [] -> str
    | c::cs -> (String.set str index c; loop(cs,index+1))
  in
    loop(cs,0)




(*
 * Create list of all strings of length <= n over a given alphabet
 *
 *)

let strings (alphabet, n) = 
  let rec mapCons (c, ls) = 
    match ls with
      [] -> []
    | l::ls' -> (c::l)::mapCons(c,ls')  in
  let rec mapConsSet (alphabet, l) = 
    match alphabet with
      [] -> []
    | c::cs -> mapCons(c,l) @ mapConsSet(cs,l)  in
  let rec mapImplode (css) = 
    match css with
      [] -> []
    | (cs::css) -> (implode cs)::mapImplode(css)  in
  let rec strings' (n) = 
    if (n<=0) then
      [[]]
    else let s = strings'(n-1) in
      [] :: mapConsSet(alphabet,s)
  in 
    mapImplode(strings'(n))






(*
 * The type for PDAs
 * 
 *)

type 'a pda = {pda_states : 'a list;
               pda_alphabet : char list;
               pda_delta : ('a * char option * char * 'a * char list) list;
               pda_start : 'a;
               pda_final : 'a list;
               pda_stack_alph : char list;
               pda_bottom : char}


let rec setIn (elt,set) = match set with 
  | [] -> false
  | h::t -> 
      if h = elt then true 
      else setIn(elt, t);;

let initial_config p w = (p.pda_start, explode(w), p.pda_bottom :: [])

let accepting_config pda cfg = let (state, inputString, stack) = cfg in 
  setIn(state, pda.pda_final) && inputString = []

let rec has_accepting_config pda cfgs = match cfgs with 
  | [] -> false
  | h::t ->
      if (accepting_config pda h) then true else has_accepting_config pda t

let rec singleDeltaStep state deltas inputChar inputRemainder originalInput stack = match deltas with 
  | [] -> []
  | h::t -> let (fromState, tranChar, requiredStackTop, toState, stackAddition) = h in 
      if state = fromState && (tranChar = None) && (List.hd stack) = requiredStackTop then
        (toState, (originalInput), (stackAddition @ (List.tl stack))) :: (singleDeltaStep state t inputChar inputRemainder originalInput stack)
      else
        if state = fromState && (inputChar = tranChar) && (List.hd stack) = requiredStackTop then
          (toState, inputRemainder, (stackAddition @ (List.tl stack))) :: (singleDeltaStep state t inputChar inputRemainder originalInput stack)
        else (singleDeltaStep state t inputChar inputRemainder originalInput stack)

let step_config pda cfg = let (state, inputlist, stack) = cfg in
  match inputlist with 
    | [] -> singleDeltaStep state pda.pda_delta None [] inputlist stack
    | h::t -> singleDeltaStep state pda.pda_delta (Some h) t inputlist stack

let rec step_configs pda cfgs = match cfgs with 
  | [] -> []
  | h::t -> (step_config pda h) @ step_configs pda t

let rec acceptHelper pda confs count = if count > 100 then false else 
  let newConfs = (step_configs pda confs) in
  if has_accepting_config pda newConfs then true
  else acceptHelper (pda) (newConfs) (count+1)

let accept pda w = acceptHelper pda ([initial_config pda w]) 0



(*
 * Create list of all strings of length <= n over a given alphabet
 *
 *)

let strings alphabet n = 
  let rec mapCons c = List.map (fun y -> c::y)  in
  let rec mapConsSet alphabet l = 
    List.fold_right (fun c -> List.append (mapCons c l)) alphabet []  in
  let rec strings' n =
    if (n<=0) then [[]]
    else [] :: mapConsSet alphabet (strings' (n-1))
  in List.map implode (strings' n)


(* 
 *  Compute the language of a PDA, restricted to inputs of length <= n
 *   language pda n   returns a list of strings accepted by dfa
 *   prin'anguage pda n   prints the strings accepted by dfa
 *
 *)

let language pda n = 
  List.filter (accept pda) (strings pda.pda_alphabet n)

let printLanguage pda n = 
  List.iter (fun s -> Printf.printf "  %s\n" (if (s="") then "<empty>" else s))
              (language pda n)


(*
 * Some sample PDAs
 *
 *)


let anban = { pda_states = ["q1"; "q2"; "q3"];
              pda_alphabet = ['a';'b'];
              pda_delta = [ ("q1", Some 'a', '_', "q1", ['.'; '_']);
			    ("q1", Some 'a', '.', "q1", ['.'; '.']);
			    ("q1", Some 'b', '_', "q2", ['_']);
			    ("q1", Some 'b', '.', "q2", ['.']);
			    ("q2", Some 'a', '.', "q2", []);
			    ("q2", None, '_', "q3", ['_'])];
	      pda_start = "q1";
	      pda_final = ["q3"];
	      pda_stack_alph = ['.'; '_'];
	      pda_bottom = '_' }

let anbn = { pda_states = ["q1"; "q2"; "q3"];
             pda_alphabet = ['a';'b'];
             pda_delta = [ ("q1", Some 'a', '_', "q1", ['.'; '_']);
			   ("q1", Some 'a', '.', "q1", ['.'; '.']);
			   ("q1", None, '_', "q2", ['_']);
			   ("q1", None, '.', "q2", ['.']);
			   ("q2", Some 'b', '.', "q2", []);
			   ("q2", None, '_', "q3", ['_'])];
	     pda_start = "q1";
	     pda_final = ["q3"];
	     pda_stack_alph = ['.'; '_'];
	     pda_bottom = '_' };;

let ambn = { pda_states = ["q1"; "q2"; "q3"];
             pda_alphabet = ['a';'b'];
             pda_delta = [ ("q1", Some 'a', 'X', "q1", ['.'; 'X']);
			   ("q1", Some 'a', '.', "q1", ['.'; '.']);
			   ("q1", None, 'X', "q2", ['X']);
			   ("q1", None, '.', "q2", ['.']);
			   ("q2", Some 'b', '.', "q2", [])]; 
	     pda_start = "q1";
	     pda_final = ["q2"];
	     pda_stack_alph = ['.'; 'X'];
	     pda_bottom = 'X' };;


initial_config anbn "aaabb";;
accepting_config anbn ("q3", [], ['.'; '.'; '_']);;
has_accepting_config anbn [("q3", [], ['.'; '.'; '_']);("q3", ['a'], ['.'; '.'; '_'])];;
has_accepting_config ambn [("q2", [], ['.'; '.'; '_']);("q2", ['a'], ['.'; '.'; '_'])];;
has_accepting_config ambn [("q2", ['b'], ['.'; '.'; '_']);("q2", ['a'], ['.'; '.'; '_'])];;
step_config anbn ("q1", ['a'; 'b'], ['_']);;
step_config anbn ("q2", ['b'], ['.'; '_']);;
step_config anbn ("q2", ['b'], ['_']);;
step_config anbn ("q3", ['a'], ['_']);;
step_config ambn ("q1",['a';'a';'b'],['.';'_']);;
step_configs anbn [("q1", ['a'; 'b'], ['_']);
                     ("q2", ['b'], ['.'; '_']);
                     ("q2", ['b'], ['_'])];;
step_configs ambn [("q1",['a';'a';'b'],['.';'_']);
                     ("q1",['b'],['.';'_'])];;

accept ambn "aa";;
printLanguage ambn 6;;