
(* FoCS Spring 2015

   Homework 7 code


   Name: Thomas Nattestad

   Email: thomas.nattestad@students.olin.edu

   Comments: These were kinda intense but also really fun. I didn't label absolutely every possible transition since I assume any failed transition is a rejected input

 *)



(* 
 *   explode_str : string -> string list
 *      returns the list of characters making up a string
 *      (where each character gets returned as a string)
 *
 *)

let explode_str (str) = 
  let rec acc (index,result) = 
    if (index<0) then
      result
    else
      acc(index-1, (String.sub str index 1)::result)
  in
    acc(String.length(str)-1, [])




(*
 * Type for deterministic Turing machines
 *
 * Parameterized by type for states
 *)

type direction = Left | Right

type symbol = string

type 'a tm = { tm_states : 'a list;
               tm_input_alph : symbol list;
	       tm_tape_alph : symbol list;
	       tm_leftmost : symbol;
	       tm_blank : symbol;
	       tm_delta : ('a * symbol * 'a * symbol * direction) list;
	       tm_start : 'a;
	       tm_accept : 'a;
	       tm_reject : 'a }


(*
 * Print a configuration (including newline) to standard output
 *  and return a value
 * 
 *)

let print_config m (u,q,v) value = 
    let print_syms = List.iter (Printf.printf "%s ")  in
    let _ = print_string "  "  in
    let _ = print_syms u  in
    let _ = Printf.printf "(%s) " q  in
    let _ = print_syms v  in
    let _ = print_newline ()  in
    value


let rec setIn (elt,set) = match set with 
  | [] -> false
  | h::t -> 
      if h = elt then true 
      else setIn(elt, t);;


(* QUESTION 1 *)

let starting_config m w = ([], m.tm_start, m.tm_leftmost::(explode_str w))

let accepting_config m c = let (_, state, _) = c in state = m.tm_accept

let rejecting_config m c = let (_, state, _) = c in state = m.tm_reject

let halting_config m c = (accepting_config m c) || (rejecting_config m c)

let rec all_but_last aList = match aList with
	| [x] -> []
	| h::t -> h :: all_but_last t

let rec last_element aList = match aList with
	| [x] -> x
	| h::t -> last_element t

(* Assumes end marker of _ *)
let rec headOfTape tape = match tape with 
	| [] -> "_"
	| h::t -> h

(* Assumes end marker of _ *)
let rec tapeTail tape = match tape with 
	| [] -> []
	| h::t -> t

let rec get_new_conf deltas c = match deltas with
	| [] -> failwith "Input Rejected"
	| h ::t -> let (startState, tranSym, nextState, newSym, direction) = h in 
		let (preTape, currentState, postTape) = c in 
			if currentState = startState && (headOfTape postTape) = tranSym then
				if direction = Right then 
					((preTape@[newSym]), nextState, (tapeTail postTape))
				else
					((all_but_last preTape), nextState, (last_element preTape)::(newSym :: (tapeTail postTape)))
			else
				get_new_conf t c

let step_config m c = get_new_conf m.tm_delta c

let rec exec m c = let newConf = step_config m c in 
	let z = print_config m c 0 in 
		if accepting_config m newConf then let z = print_config m newConf 0 in true 
		else if rejecting_config m newConf then let z = print_config m newConf 0 in false 
		else exec m newConf

let run m w = let conf = starting_config m w in exec m conf




(* 
 * Some sample deterministic Turing machines
 *
 * asbs is the regular language {a^m b^n | m,n >= 0}
 * anbn is the context-free language {a^n b^n | n >= 0}
 * anbncn is the non-context-free language {a^n b^n c^n | n >= 0}
 *
 *)

let asbs = { tm_states = ["start"; "q1"; "acc"; "rej"];
	     tm_input_alph = ["a";"b"];
	     tm_tape_alph = ["a";"b";"_";">"];
	     tm_blank = "_";
	     tm_leftmost = ">";
	     tm_start = "start";
	     tm_accept = "acc";
	     tm_reject = "rej";
	     tm_delta = [("start", "a", "start", "a", Right);
     	                ("start", "b", "q1", "b", Right);
		        ("start", ">", "start", ">", Right);
		        ("start", "_", "acc", "_", Right);
		        ("q1", "a", "rej", "a", Right);
		        ("q1", "b", "q1", "b", Right);
		        ("q1", ">", "rej", ">", Right);
		        ("q1", "_", "acc", "_", Right);
		        ("acc", "a", "acc", "a", Right);
		        ("acc", "b", "acc", "b", Right);
		        ("acc", ">", "acc", ">", Right);
		        ("acc", "_", "acc", "_", Right);
		        ("rej", "a", "rej", "a", Right);
		        ("rej", "b", "rej", "b", Right);
		        ("rej", ">", "rej", ">", Right);
		        ("rej", "_", "rej", "_", Right)] }

let anbn = { tm_states = ["start"; "q1"; "q2"; "q3"; "q4"; "acc"; "rej"];
	     tm_input_alph = ["a";"b"];
	     tm_tape_alph = ["a";"b";"X";"_";">"];
	     tm_blank = "_";
	     tm_leftmost = ">";
	     tm_start = "start";
	     tm_accept = "acc";
	     tm_reject = "rej";
	     tm_delta = [ ("start", "a", "start", "a", Right);
     	                ("start", "b", "q1", "b", Right);
		        ("start", ">", "start", ">", Right);
		        ("start", "_", "q2", "_", Right);
		        ("start", "X", "rej", "X", Right);
		        ("q1", "b", "q1", "b", Right);
		        ("q1", "_", "q2", "_", Right);
		        ("q1", "a", "rej", "a", Right);
		        ("q1", ">", "rej", ">", Right);
		        ("q1", "X", "rej", "X", Right);
		        ("q2", ">", "q3", ">", Right);
		        ("q2", "a", "q2", "a", Left);
		        ("q2", "b", "q2", "b", Left);
		        ("q2", "X", "q2", "X", Left);
		        ("q2", "_", "q2", "_", Left);
		        ("q3", "X", "q3", "X", Right);
		        ("q3", "_", "acc", "_", Right);
		        ("q3", "a", "q4", "X", Right);
		        ("q3", "b", "rej", "b", Right);
		        ("q3", ">", "rej", ">", Right);
		        ("q4", "a", "q4", "a", Right);
		        ("q4", "X", "q4", "X", Right);
		        ("q4", "b", "q2", "X", Right);
		        ("q4", "a", "rej", "a", Right);
		        ("q4", ">", "rej", ">", Right);
		        ("q4", "_", "rej", "_", Right);
		        ("acc", "a", "acc", "a", Right);
		        ("acc", "b", "acc", "b", Right);
		        ("acc", ">", "acc", ">", Right);
		        ("acc", "X", "acc", "X", Right);
		        ("acc", "_", "acc", "_", Right);
		        ("rej", "a", "rej", "a", Right);
		        ("rej", "b", "rej", "b", Right);
		        ("rej", ">", "rej", ">", Right);
		        ("rej", "X", "rej", "X", Right);
		        ("rej", "_", "rej", "_", Right)] }

let anbncn = { tm_states = ["start";"q1";"q2";"q3";"q4";"q5";"q6";"acc";"rej"];
	       tm_input_alph = ["a";"b";"c"];
	       tm_tape_alph = ["a";"b";"c";"X";"_";">"];
	       tm_blank = "_";
	       tm_leftmost = ">";
	       tm_start = "start";
	       tm_accept = "acc";
	       tm_reject = "rej";
	       tm_delta = [ ("start", "a", "start", "a", Right);
     	          ("start", "b", "q1", "b", Right);
		  ("start", "c", "q6", "c", Right);
		  ("start", ">", "start", ">", Right);
		  ("start", "_", "q2", "_", Right);
		  ("start", "X", "rej", "X", Right);
		  ("q1", "b", "q1", "b", Right);
		  ("q1", "c", "q6", "c", Right);
		  ("q1", "_", "q2", "_", Right);
		  ("q1", "a", "rej", "a", Right);
		  ("q1", ">", "rej", ">", Right);
		  ("q1", "X", "rej", "X", Right);
		  ("q2", ">", "q3", ">", Right);
		  ("q2", "a", "q2", "a", Left);
		  ("q2", "b", "q2", "b", Left);
		  ("q2", "c", "q2", "c", Left);
		  ("q2", "_", "q2", "_", Left);
		  ("q2", "X", "q2", "X", Left);
		  ("q3", "X", "q3", "X", Right);
		  ("q3", "_", "acc", "_", Right);
		  ("q3", "a", "q4", "X", Right);
		  ("q3", "b", "rej", "b", Right);
		  ("q3", "c", "rej", "c", Right);
		  ("q3", ">", "rej", ">", Right);
		  ("q4", "a", "q4", "a", Right);
		  ("q4", "X", "q4", "X", Right);
		  ("q4", "b", "q5", "X", Right);
		  ("q4", "c", "rej", "c", Right);
		  ("q4", "_", "rej", "_", Right);
		  ("q4", ">", "rej", ">", Right);
		  ("q5", "b", "q5", "b", Right);
		  ("q5", "X", "q5", "X", Right);
		  ("q5", "c", "q2", "X", Right);
		  ("q5", "a", "rej", "a", Right);
		  ("q5", "_", "rej", "_", Right);
		  ("q5", ">", "rej", ">", Right);
		  ("q6", "c", "q6", "c", Right);
		  ("q6", "_", "q2", "_", Right);
		  ("q6", "a", "rej", "a", Right);
		  ("q6", "b", "rej", "b", Right);
		  ("q6", ">", "rej", ">", Right);
		  ("q6", "X", "rej", "X", Right);
		  ("acc", "a", "acc", "a", Right);
		  ("acc", "b", "acc", "b", Right);
		  ("acc", "c", "acc", "c", Right);
		  ("acc", ">", "acc", ">", Right);
		  ("acc", "X", "acc", "X", Right);
		  ("acc", "_", "acc", "_", Right);
		  ("rej", "a", "rej", "a", Right);
		  ("rej", "b", "rej", "b", Right);
		  ("rej", "c", "rej", "c", Right);
		  ("rej", ">", "rej", ">", Right);
		  ("rej", "X", "rej", "X", Right);
		  ("rej", "_", "rej", "_", Right)] }




(* QUESTION 2 *)

let question2a = { tm_states = ["start"; "q0"; "bcheck"; "q1"; "rewind"; "q12"; "q13"; "q3"; "q4"; "q5"; "q6"; "q7"; "q41"; "q61"; "q62"];
	     tm_input_alph = ["a";"b"];
	     tm_tape_alph = ["a";"b";"X";"_";">"];
	     tm_blank = "_";
	     tm_leftmost = ">";
	     tm_start = "start";
	     tm_accept = "acc";
	     tm_reject = "rej";
	     tm_delta = [ 
	     		("start", "a", "q0", "a", Left);
				("start", "b", "bcheck", "X", Right);
	     		("start", ">", "start", ">", Right);
	     		("start", "X", "start", "X", Right);
	     		("start", "_", "acc", "_", Right);

	     		("bcheck", "a", "rej", "a", Right);
	     		("bcheck", "b", "start", "X", Right);
	     		("bcheck", "X", "rej", "", Right);
	     		("bcheck", ">", "rej", "", Right);
	     		("bcheck", "_", "rej", "", Right);

	     		(* Check for all a until b->q1 *)
	     		("q0", "a", "q0", "a", Right);
     	        ("q0", "b", "q1", "b", Right);
		        ("q0", ">", "q0", ">", Right);
		        ("q0", "_", "rewind", "_", Right);
		        ("q0", "X", "rej", "X", Right);
		        
		        (* Check for all b until a -> q12 *)
		        ("q1", "b", "q1", "b", Right);
		        ("q1", "a", "q12", "a", Right);
		        ("q1", "_", "rewind", "_", Right); (* todo: fix bb *)
		        ("q1", ">", "rej", ">", Right);
		        ("q1", "X", "rej", "X", Right);

		        (* Check for all a until b -> q13*)
	     		("q12", "a", "q12", "a", Right);
     	        ("q12", "b", "q13", "b", Right);
		        ("q12", ">", "rej", ">", Right);
		        ("q12", "_", "rewind", "_", Right);
		        ("q12", "X", "rej", "X", Right);

		        (* Check for all b until _ -> *)
		        ("q13", "b", "q13", "b", Right);
     	        ("q13", "a", "rej", "a", Right);
		        ("q13", ">", "rej", ">", Right);
		        ("q13", "_", "rewind", "_", Right);
		        ("q13", "X", "rej", "X", Right);

		        (* rewind until > *)
		        ("rewind", ">", "q3", ">", Right);
		        ("rewind", "a", "rewind", "a", Left);
		        ("rewind", "b", "rewind", "b", Left);
		        ("rewind", "X", "rewind", "X", Left);
		        ("rewind", "_", "rewind", "_", Left);
		        
		        (* Mark a=X->q4, accept if _ , reject if b *)
		        ("q3", "X", "q3", "X", Right);
		        ("q3", "_", "acc", "_", Right);
		        ("q3", "a", "q4", "X", Right);
		        ("q3", "b", "q6", "X", Right);
		        ("q3", ">", "rej", ">", Right);
		        
		        (* Mark b=X->rewind, reject if a > or _ *)
		        ("q4", "a", "q4", "a", Right);
		        ("q4", "X", "q41", "X", Left);
		        ("q4", "b", "q5", "b", Right);
		        ("q4", ">", "rej", ">", Right);
		        ("q4", "_", "q41", "_", Left);

		        ("q41", "a", "rewind", "X", Right);
		        ("q41", "b", "rej", "X", Right);
		        ("q41", "X", "rej", "X", Right);
		        ("q41", ">", "rej", "X", Right);
		        ("q41", "_", "rej", "X", Right);

		        (* Loop b,X until a=X-> rewind *)
		        ("q5", "a", "rewind", "X", Right);
		        ("q5", "b", "q5", "b", Right);
		        ("q5", "X", "q5", "X", Right);
		        ("q5", ">", "rej", ">", Right);
		        ("q5", "_", "rej", "_", Right);

		        (* Loop b until X=X->q7 *)
		        ("q6", "a", "rej", "a", Right);
		        ("q6", "b", "q6", "b", Right);
		        ("q6", "X", "q7", "X", Right);
		        ("q6", ">", "rej", ">", Right);
		        ("q6", "_", "q7", "_", Right);

		        (* Loop X until b=X->rewind *)
		        ("q7", "a", "rej", "a", Right);
		        ("q7", "b", "rewind", "X", Right);
		        ("q7", "X", "q7", "X", Right);
		        ("q7", ">", "rej", ">", Right);
		        ("q7", "_", "rej", "_", Right);
		        
		        (* Accepts/Rejects *)
		        ("acc", "a", "acc", "a", Right);
		        ("acc", "b", "acc", "b", Right);
		        ("acc", ">", "acc", ">", Right);
		        ("acc", "X", "acc", "X", Right);
		        ("acc", "_", "acc", "_", Right);
		        ("rej", "a", "rej", "a", Right);
		        ("rej", "b", "rej", "b", Right);
		        ("rej", ">", "rej", ">", Right);
		        ("rej", "X", "rej", "X", Right);
		        ("rej", "_", "rej", "_", Right)] };;

let question2b = { tm_states = ["start"; "q1"; "checkAllB"; "checkAllB2"; "rewind"; "q2"; "q3"; "q4"; "rewindB"; "q5"];
	     tm_input_alph = ["a";"b"];
	     tm_tape_alph = ["a";"b"; "B";"X";"_";">"];
	     tm_blank = "_";
	     tm_leftmost = ">";
	     tm_start = "start";
	     tm_accept = "acc";
	     tm_reject = "rej";
	     tm_delta = [ 
	     		(* Check for all a until b->q1 *)
	     		("start", "a", "start", "a", Right);
     	        ("start", "b", "q1", "b", Right);
		        ("start", ">", "start", ">", Right);
		        ("start", "_", "acc", "_", Right);
		        ("start", "X", "rej", "X", Right);
		        
		        ("q1", "a", "q11", "a", Right);
		        ("q1", "b", "q1", "b", Right);
		        ("q1", "_", "checkAllB", "_", Left);
		        ("q1", ">", "rej", ">", Right);
		        ("q1", "X", "rej", "X", Right);

		        ("checkAllB", "a", "checkAllB", "a", Left);
		        ("checkAllB", "b", "checkAllB", "b", Left);
		        ("checkAllB", ">", "checkAllB2", ">", Right);
		        ("checkAllB", "X", "rej", "X", Left);
		        ("checkAllB", "B", "rej", "B", Left);
		        ("checkAllB", "_", "rej", "_", Left);

		        ("checkAllB2", "a", "rej", "a", Right);
		        ("checkAllB2", "b", "checkAllB2", "b", Right);
		        ("checkAllB2", "_", "acc", "_", Right);
		        
		        ("q11", "a", "q11", "a", Right);
		        ("q11", "b", "rej", "b", Right);
		        ("q11", "_", "rewind", "_", Right);
		        ("q11", ">", "rej", ">", Right);
		        ("q11", "X", "rej", "X", Right);

		        ("rewind", "a", "rewind", "a", Left);
		        ("rewind", "b", "rewind", "b", Left);
		        ("rewind", "B", "rewind", "b", Left);
		        ("rewind", "_", "rewind", "_", Left);
		        ("rewind", ">", "q2", ">", Right);
		        ("rewind", "X", "rewind", "X", Left);

		        ("q2", "a", "q3", "X", Right);
		        ("q2", "b", "q5", "b", Right);
		        ("q2", "_", "acc", "_", Right);
		        ("q2", ">", "rej", ">", Right);
		        ("q2", "X", "q2", "X", Right);

		        ("q3", "a", "q3", "a", Right);
		        ("q3", "b", "q4", "B", Right);
		        ("q3", "_", "rej", "_", Right);
		        ("q3", ">", "rej", ">", Right);
		        ("q3", "X", "rewind", "X", Right);
		        ("q3", "B", "q3", "B", Right);

		        ("q4", "a", "rewindB", "X", Left);
		        ("q4", "b", "q4", "b", Right);
		        ("q4", "X", "q4", "X", Right);
		        ("q4", "_", "rej", "_", Right);
		        ("q4", ">", "rej", ">", Right);

		        ("rewindB", "a", "rej", "a", Left);
		        ("rewindB", "b", "rewindB", "b", Left);
		        ("rewindB", "B", "q3", "B", Right);
		        ("rewindB", ">", "rej", ">", Left);
		        ("rewindB", "_", "rej", "_", Left);
		        ("rewindB", "X", "rewindB", "X", Left);

		        ("q5", "X", "q5", "X", Right);
		        ("q5", "b", "q5", "b", Right);
		        ("q5", "_", "acc", "_", Right);
		        ("q5", "a", "rej", "a", Right);
		        ("q5", "B", "rej", "B", Right);
		        ("q5", ">", "rej", ">", Right);

		        ("acc", "a", "acc", "a", Right);
		        ("acc", "b", "acc", "b", Right);
		        ("acc", ">", "acc", ">", Right);
		        ("acc", "X", "acc", "X", Right);
		        ("acc", "_", "acc", "_", Right);
		        ("rej", "a", "rej", "a", Right);
		        ("rej", "b", "rej", "b", Right);
		        ("rej", ">", "rej", ">", Right);
		        ("rej", "X", "rej", "X", Right);
		        ("rej", "_", "rej", "_", Right)
		        ] }



(* QUESTION 3 *)

let binary_sum = { tm_states = ["start"; "q1"; "q2"; "q3"; "q4"; "q5"; "q6"; "q7"; "q8"; "rewind"; "q9"];
	     tm_input_alph = ["0"; "1"; "#"];
	     tm_tape_alph = ["0"; "1";"a";"b";"#";"_";">"];
	     tm_blank = "_";
	     tm_leftmost = ">";
	     tm_start = "start";
	     tm_accept = "acc";
	     tm_reject = "rej";
	     tm_delta = [ 
	     		("start", "0", "start", "0", Right);
	     		("start", "1", "start", "1", Right);
	     		("start", "a", "start", "a", Right);
	     		("start", "b", "start", "b", Right);
	     		("start", "#", "start", "#", Right);
				("start", "X", "start", "X", Right);
	     		("start", "_", "q1", "_", Left);
	     		("start", ">", "start", ">", Right);

	     		("q1", "0", "q2", "X", Left);
	     		("q1", "1", "q4", "X", Left);
	     		("q1", "a", "rej", "a", Right);
	     		("q1", "b", "rej", "b", Right);
	     		("q1", "#", "rewind", "#", Right);
				("q1", "X", "q1", "X", Left);
	     		("q1", "_", "rej", "_", Right);
	     		("q1", ">", "rej", ">", Right);

	     		("q2", "0", "q2", "0", Left);
	     		("q2", "1", "q2", "1", Left);
	     		("q2", "a", "rej", "a", Right);
	     		("q2", "b", "rej", "b", Right);
	     		("q2", "#", "q3", "#", Left);
				("q2", "X", "rej", "X", Right);
	     		("q2", "_", "rej", "_", Right);
	     		("q2", ">", "rej", ">", Right);
	     		
	     		("q3", "0", "start", "a", Right);
	     		("q3", "1", "start", "b", Right);
	     		("q3", "a", "q3", "a", Left);
	     		("q3", "b", "q3", "b", Left);
	     		("q3", "#", "rej", "#", Right);
				("q3", "X", "rej", "X", Right);
	     		("q3", "_", "rej", "_", Right);
	     		("q3", ">", "rej", ">", Right);

	     		("q4", "0", "q4", "0", Left);
	     		("q4", "1", "q4", "1", Left);
	     		("q4", "a", "rej", "a", Right);
	     		("q4", "b", "rej", "b", Right);
	     		("q4", "#", "q5", "#", Left);
				("q4", "X", "rej", "X", Right);
	     		("q4", "_", "rej", "_", Right);
	     		("q4", ">", "rej", ">", Right);

	     		("q5", "0", "start", "b", Right);
	     		("q5", "1", "q6", "a", Left);
	     		("q5", "a", "q5", "a", Left);
	     		("q5", "b", "q5", "b", Left);
	     		("q5", "#", "rej", "#", Right);
				("q5", "X", "rej", "X", Left);
	     		("q5", "_", "rej", "_", Right);
	     		("q5", ">", "rej", ">", Right);

	     		("q6", "0", "start", "1", Right);
	     		("q6", "1", "q6", "1", Left);
	     		("q6", "a", "rej", "a", Right);
	     		("q6", "b", "rej", "b", Right);
	     		("q6", "#", "rej", "#", Right);
				("q6", "X", "rej", "X", Right);
	     		("q6", "_", "rej", "_", Right);
	     		("q6", ">", "rej", ">", Right);

	     		("rewind", "0", "rewind", "0", Left);
	     		("rewind", "1", "rewind", "1", Left);
	     		("rewind", "a", "rewind", "a", Left);
	     		("rewind", "b", "rewind", "b", Left);
	     		("rewind", "#", "rewind", "#", Left);
				("rewind", "X", "rewind", "X", Left);
	     		("rewind", "_", "rewind", "_", Left);
	     		("rewind", ">", "q9", ">", Right);

				("q9", "0", "q8", "X", Right);
	     		("q9", "1", "q7", "X", Right);
	     		("q9", "a", "rej", "a", Right);
	     		("q9", "b", "rej", "b", Right);
	     		("q9", "#", "q9", "#", Right);
				("q9", "X", "q9", "X", Right);
	     		("q9", "_", "acc", "_", Right);
	     		("q9", ">", "rej", ">", Right);

	     		("q7", "0", "q7", "0", Right);
	     		("q7", "1", "q7", "1", Right);
	     		("q7", "a", "rej", "a", Right);
	     		("q7", "b", "rewind", "X", Right);
	     		("q7", "#", "q7", "#", Right);
				("q7", "X", "q7", "X", Right);
	     		("q7", "_", "rej", "_", Right);
	     		("q7", ">", "rej", ">", Right);

	     		("q8", "0", "q8", "0", Right);
	     		("q8", "1", "q8", "1", Right);
	     		("q8", "a", "rewind", "X", Right);
	     		("q8", "b", "rej", "b", Right);
	     		("q8", "#", "q8", "#", Right);
				("q8", "X", "q8", "X", Right);
	     		("q8", "_", "rej", "_", Right);
	     		("q8", ">", "rej", ">", Right);

		        ] };;

(* step_config anbn (["a"], "q2", ["a"; "b"]);; *)
(* run asbs "aab";; *)
(* run anbn "aabb";; *)
(* run anbncn "aabbcc";; *)
(* run question2a "bb" *)
(* run question2b "abbbaaa" *)
(* run binary_sum "0110#0100#0010";;  *)




