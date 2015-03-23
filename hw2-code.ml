(* FoCS Spring 2015

   Homework 2 code


   Name: Thomas Nattestad

   Email: thomas.nattestad@students.olin.edu

   Comments: I feel like most of the good practices I have picked up such as printing my inbetween inputs to check myself and testing subcomponents is incredibly difficult if not impossible. Is there a way to do these checks?

 *)


(* My Helper Functions *)
let rec setIn (elt,set) = match set with 
  | [] -> false
  | h::t -> 
      if h = elt then true 
      else setIn(elt, t);;

(* 
 * The type for a DFA, parameterized by the type for the states 
 *
 *)

type 'a dfa = {states : 'a list;
    	       alphabet : char list;
	              start : 'a;
   	            delta : ('a * char * 'a) list;
	              final : 'a list}

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
 * Some error code
 * Call "transitionError" to report an error while looking for a 
 *   transition in the delta of a DFA
 *
 *)

let transitionError (input) = 
  failwith ("DFA Error: Cannot transition on input "^(implode [input]))

(*
 * Some sample DFAs
 *
 *)


let isolatedBs =                (* language: all strings where every b *)
  {alphabet = ['a'; 'b'];       (* is bracketed by a's                 *)   
   states = ["start"; "readb"; "sink"];
   start = "start";
   delta = [("start", 'a', "start");
            ("start", 'b', "readb");
            ("readb", 'a', "start");
            ("readb", 'b', "sink");
            ("sink", 'a', "sink");
            ("sink", 'b', "sink")];
   final = ["start";"readb"]}


let ambn =                 (* language: strings of a's followed by b's *)
    {states = ["eata"; "eatb"; "sink"];
     alphabet = ['a'; 'b'];
     start = "eata";
     delta = [("eata", 'a', "eata");
              ("eata", 'b', "eatb");
              ("eatb", 'a', "sink");
              ("eatb", 'b', "eatb");
              ("sink", 'a', "sink");
              ("sink", 'b', "sink")];
     final = ["eata"; "eatb"]}


let aStar =                    (* language: all strings of a's *)
  {alphabet= ['a'; 'b'];
   states= ["ok"; "sink"];
   start= "ok";
   delta = [("ok",   'a', "ok");
            ("ok",   'b', "sink");
            ("sink", 'a', "sink");
            ("sink", 'b', "sink")];
   final = ["ok"]}


let bPlus =                     (* language: all nonempty strings of b's *)
  {alphabet= ['a'; 'b'];
   states= ["start"; "ok"; "sink"];
   start= "start";
   delta = [("start", 'b', "ok");
            ("start", 'a', "sink");
            ("ok",    'b', "ok");
            ("ok",    'a', "sink");
            ("sink",  'b', "sink");
            ("sink",  'a', "sink")];
   final = ["ok"]}


let abaStar =              (* language: any number of ab's followed by a's *)
  {alphabet= ['a'; 'b'];
   states= ["astate"; "bstate"; "aonly"; "sink"];
   start= "astate";
   delta = [("astate", 'a', "bstate");
            ("astate", 'b', "sink");
            ("bstate", 'a', "aonly");
            ("bstate", 'b', "astate");
            ("aonly",  'a', "aonly");
            ("aonly",  'b', "sink");
            ("sink",   'a', "sink");
            ("sink",   'b', "sink")];
   final = ["astate"; "bstate"; "aonly"]}



(*
 * Create list of all strings of length <= n over a given alphabet
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
    mapImplode(strings'(n));;


(*
 *  isFinal : 'a dfa * 'a -> bool
 *
 *    isFinal(dfa,q) should return true if and only if 'q' is a final state
 *    in the DFA 'dfa'
 *
 *  PROVIDE CODE FOR THIS FUNCTION FOR QUESTION (2) 
 *
 *)

let isFinal (dfa,state) = 
  setIn(state, dfa.final);;


(* 
 *  transition : 'a dfa * 'a * char -> 'a
 *
 *    transition(dfa,q,a) should return the state obtained by reading input
 *    symbol 'a' in state 'q' in the DFA 'dfa'
 *
 *  PROVIDE CODE FOR THIS FUNCTION FOR QUESTION (2) 
 *
 *)

let rec getTransition(deltas, state, input) = match deltas with
  | [] -> transitionError(input)
  | h::t -> 
    let (start,tran,terminal) = h in 
      if start = state && tran = input then 
        terminal
      else
        getTransition(t, state, input)

let transition (dfa,state,input) = 
  getTransition(dfa.delta, state, input);;

let rec extendedTransition (dfa, state, cs) = match cs with
  | [] -> dfa.start
  | [a] -> getTransition(dfa.delta, state, a)
  | h::t ->
    let newState = getTransition(dfa.delta, state, h) in 
      extendedTransition(dfa, newState, t)

let accept (dfa, input) = 
  setIn(extendedTransition(dfa, dfa.start, explode(input)), dfa.final)



(*
 * PLACE YOUR ANSWERS TO QUESTION 3 HERE
 *
 * Each of these should be a function of no argument
 * returning the DFA that is a solution to the question
 *
 *)
(* All strings of length 3 *)
let dfaQuestion1a () = 
  {alphabet = ['a'; 'b'];       (* is bracketed by a's                 *)   
   states = ["start"; "first"; "second"; "third"];
   start = "start";
   delta = [("start", 'a', "first");
            ("start", 'b', "first");
            ("first", 'a', "second");
            ("first", 'b', "second");
            ("second", 'a', "third");
            ("second", 'b', "third");
            ("third", 'a', "sink");
            ("third", 'b', "sink");
            ("sink", 'a', "sink");
            ("sink", 'b', "sink")];
   final = ["start";"third"]}

let dfaQuestion1b () = 
  {alphabet = ['a'; 'b'];       (* is bracketed by a's                 *)   
   states = ["start"; "first"; "second"; "third"];
   start = "start";
   delta = [("start", 'a', "first");
            ("start", 'b', "first");
            ("first", 'a', "second");
            ("first", 'b', "second");
            ("second", 'a', "third");
            ("second", 'b', "third");
            ("third", 'a', "first");
            ("third", 'b', "first")];
   final = ["start";"third"]}

let dfaQuestion1c () = 
  {alphabet = ['a'; 'b'];       (* is bracketed by a's                 *)   
   states = ["start"; "first"];
   start = "start";
   delta = [("start", 'a', "first");
            ("start", 'b', "start");
            ("first", 'a', "start");
            ("first", 'b', "first")];
   final = ["start"]}
  

let dfaQuestion1d () = 
    {alphabet = ['a'; 'b'];       (* is bracketed by a's                 *)   
   states = [1; 2; 3; 4];
   start = 1;
   delta = [(1, 'a', 1);
            (1, 'b', 2);
            (2, 'a', 4);
            (2, 'b', 3);
            (3, 'a', 3);
            (3, 'b', 3);
            (4, 'a', 1);
            (4, 'b', 2)];
   final = [4]}

let dfaQuestion1e () = 
    {alphabet = ['a'; 'b'];       (* is bracketed by a's                 *)   
   states = [1; 2; 3; 4; 5; 6; 7; 8; 9];
   start = 1;
   delta = [(1, 'a', 2);
            (1, 'b', 2);
            (2, 'a', 3);
            (2, 'b', 3);
            (3, 'a', 4);
            (3, 'b', 4);
            (4, 'a', 5);
            (4, 'b', 5);
            (5, 'a', 6);
            (5, 'b', 6);
            (6, 'a', 7);
            (6, 'b', 7);
            (7, 'a', 8);
            (7, 'b', 8);
            (8, 'a', 9);
            (8, 'b', 9);
            (9, 'a', 4);
            (9, 'b', 4)];
   final = [1; 3; 4; 5; 7; 9]}




(* 
 *  Compute the language of a DFA, restricted to inputs of length <= n
 *   language(dfa,n) returns a list of strings accepted by dfa
 *   printLanguage(dfa,n) prints the strings accepted by dfa
 *
 *)

let language (dfa, n) = 
  let candidates = strings(dfa.alphabet, n) in
  let rec tryAll (l) = 
    match l with
      [] -> []
    | s::ss -> if (accept(dfa,s)) then
                 s::(tryAll ss)
               else
                 tryAll ss
  in
    tryAll(candidates)


let printLanguage (dfa,n) = 
  let rec printList (l) = 
    match l with 
      [] -> ()
    | s::ss -> (print_string "   ";
                if (s="") then
                  print_string "<empty>"
                else
                  print_string s; 
                print_newline(); 
                printList ss)
  in
    printList(language(dfa,n))



(* Tests *)
(* (* let testing1 = dfaQuestion1a();;
printLanguage(testing1,10);;

let testing2 = dfaQuestion1b();;
printLanguage(testing2,10);; *)

let testing3 = dfaQuestion1c();;
printLanguage(testing3,10);;

(* let testing4 = dfaQuestion1d();;
printLanguage(testing4, 10);; *)

(* let testing5 = dfaQuestion1e();;
printLanguage(testing5, 9);; *) *)
