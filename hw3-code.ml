(* FoCS Spring 2015

   Homework 3 code


   Name: Thomas Nattestad

   Email: thomas.nattestad@students.olin.edu

   Comments: Fun assignment, liked basically all of it, though it did get a bit messey with so many functions 

 *)




(* 
 * The type for a DFA, parameterized by the type for the states 
 *
 *)

type 'a dfa = {states :   'a list;
    	       alphabet : char list;
	       start :    'a;
   	       delta :    ('a * char * 'a) list;
	       final :    'a list}


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


let aStar =                    
  (* language: all strings of a's *)
  {alphabet= ['a'; 'b'];
   states= ["ok"; "sink"];
   start= "ok";
   delta = [("ok",   'a', "ok");
            ("ok",   'b', "sink");
            ("sink", 'a', "sink");
            ("sink", 'b', "sink")];
   final = ["ok"]}


let bPlus =                     
  (* language: all nonempty strings of b's *)
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
 *  isFinal : 'a dfa * 'a -> bool
 *
 *    isFinal(dfa,q) should return true if and only if 'q' is a final state
 *    in the DFA 'dfa'
 *
 *  PROVIDE CODE FOR THIS FUNCTION FOR QUESTION (2) 
 *
 *)

let rec setIn (elt,set) = match set with 
  | [] -> false
  | h::t -> 
      if h = elt then true 
      else setIn(elt, t);;

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

(*
 *  extendedTransition : 'a dfa * 'a * char list -> 'a
 *
 *    extendedTransition(dfa,q,cs) should return the state obtained by
 *    reading the list of input symbols in 'cs' from state 'q' in the DFA
 *    'dfa'
 *
 *  PROVIDE CODE FOR THIS FUNCTION FOR QUESTION (2) 
 *
 *)

let rec extendedTransition (dfa, state, cs) = match cs with
  | [] -> dfa.start
  | [a] -> getTransition(dfa.delta, state, a)
  | h::t ->
    let newState = getTransition(dfa.delta, state, h) in 
      extendedTransition(dfa, newState, t)

let rec setIn (elt,set) = match set with 
  | [] -> false
  | h::t -> 
      if h = elt then true 
      else setIn(elt, t);;

let accept (dfa, input) = 
  setIn(extendedTransition(dfa, dfa.start, explode(input)), dfa.final)




(*
 * Exercise 2 
 *
 *)

let rec subtractSets (set1, set2) = match set1 with 
  | [] -> []
  | h::t -> 
      if setIn(h, set2) then subtractSets(t, set2) else h :: subtractSets(t, set2)

let complement (dfa) = 
  {alphabet = dfa.alphabet;
   states = dfa.states;
   start = dfa.start;
   delta = dfa.delta;
   final = subtractSets(dfa.states, dfa.final)}

let rec elementCross (ele, set) = match set with 
  | [] -> []
  | h::t -> 
      (ele, h) :: elementCross(ele, t)

let rec append(xs, ys) = match xs with
  | [] -> ys
  | h :: t -> h :: append(t, ys);;

let rec cross (xs, ys) = match xs with
  | [] -> []
  | h::t -> append(elementCross(h, ys), cross(t, ys));;

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

let rec deltasOfAlphabet(dfa1, dfa2, startState, alphabet) = match alphabet with 
  | [] -> []
  | symbol::t -> 
      let (first, second) = startState in 
        (startState, symbol, (transition(dfa1, first, symbol), transition(dfa2, second, symbol))) :: deltasOfAlphabet(dfa1, dfa2, startState, t)

let rec getDeltas(dfa1, dfa2, alphabet, crossed) = match crossed with 
  | [] -> []
  | h::t -> append(deltasOfAlphabet(dfa1, dfa2, h, alphabet) , getDeltas(dfa1, dfa2, alphabet, t))

let rec getUniqueSet(set) = match set with
  | [] -> []
  | h::t -> 
      if setIn(h, t) then getUniqueSet(t)
      else h :: getUniqueSet(t);;

let union (dfa1, dfa2) = 
  {alphabet = append(dfa1.alphabet, dfa2.alphabet);
   states = cross(dfa1.states, dfa2.states);
   start = (dfa1.start, dfa2.start);
   delta = getUniqueSet(getDeltas(dfa1, dfa2, append(dfa1.alphabet, dfa2.alphabet), cross(dfa1.states, dfa2.states)));
   final = append(cross(dfa1.final, dfa2.states), cross(dfa2.final, dfa1.states))
  }

(*
 *  Set-based helper functions, mostly from Homework 1
 *
 *  The only addition is 'subsets', which computes the
 *  set of subsets of a set (all taken to be lists, 
 *  of course)
 *
 *  'subsets' should be handy for Exercise 4
 *
 *)


let rec setIn (elt,set) = 
  match set with
    [] -> false
  | x::xs -> (x = elt || setIn(elt,xs))

let rec setSub (set1,set2) = 
  match set1 with
    [] -> true
  | x::xs -> setIn(x,set2) && setSub(xs,set2)

let setEqual (set1,set2) = 
  setSub(set1,set2) && setSub(set2,set1)

let rec setInter (set1,set2) = 
   match set1 with 
     [] -> []
   | x::xs -> if setIn(x,set2) then 
                 x::(setInter(xs,set2))
              else 
                 setInter(xs,set2)

let rec subsets xs = 
  match xs with
    [] -> [[]]
  | x::xs' -> subsets(xs') @ (List.map (fun ys -> x::ys) (subsets xs'))

let rec append(xs, ys) = match xs with
  | [] -> ys
  | h :: t -> h :: append(t, ys);;


(* 
 * The type for an NFA, parameterized by the type for the states 
 *
 *)

type 'a nfa = {nfa_states :   'a list;
               nfa_alphabet : char list;
               nfa_start :    'a;
   	       nfa_delta :    ('a * char * 'a list) list;
	       nfa_final :    'a list}


(*
 * Some sample NFAs
 *
 *)


(* language: (ab+aab)* *)
let abaabStar = {nfa_states = ["s";"a11";"a21";"a22"];
                 nfa_alphabet = ['a';'b'];
                 nfa_start = "s";
                 nfa_delta = [("s", 'a', ["a11"; "a21"]);
                               ("a21", 'a', ["a22"]);
                               ("a11", 'b', ["s"]);
                               ("a22", 'b', ["s"])];
                 nfa_final = ["s"]}

(* language: ab+aab *)
let abaab = {nfa_states = ["q1"; "q2"; "q3"; "q4"; "q5"];
             nfa_alphabet = ['a'; 'b'];
             nfa_start = "q1";
             nfa_delta = [("q1", 'a', ["q2"; "q3"]);
                        ("q2", 'b', ["q5"]);
                        ("q3", 'a', ["q4"]);
                        ("q4", 'b', ["q5"])];
             nfa_final = ["q5"]}





(*
 * In common for exercises 3 and 4
 *
 *)


let rec nfa_hasFinal (nfa,states) = match states with 
  | [] -> false
  | h::t -> if(setIn(h, nfa.nfa_final)) then true else nfa_hasFinal(nfa, t);;

let rec getTransition(deltas, state, input) = match deltas with
  | [] -> []
  | h::t -> 
    let (start,tran,terminals) = h in 
      if start = state && tran = input then 
        terminals
      else
        getTransition(t, state, input)

let singleTransition (nfa,state,input) = 
  getTransition(nfa.nfa_delta, state, input);;

let rec nfa_transition (nfa,states,input) = match states with 
  | [] -> []
  | aState::t -> append(singleTransition(nfa, aState, input), nfa_transition(nfa, t, input))
  
let rec nfa_extendedTransition (nfa, states, cs) = match cs with
  | [] -> [nfa.nfa_start]
  | [a] -> nfa_transition(nfa, states, a)
  | h::t ->
    let newStates = nfa_transition(nfa, states, h) in 
      nfa_extendedTransition(nfa, newStates, t)

let nfa_accept (nfa, input) = 
  nfa_hasFinal(nfa, nfa_extendedTransition(nfa, [nfa.nfa_start], explode(input)))



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


(* 
 *  Compute the language of an NFA, restricted to inputs of length <= n
 *   nfa_language(nfa,n) returns a list of strings accepted by nfa
 *   nfa_printLanguage(nfa,n) prints the strings accepted by nfa
 *
 *)

let nfa_language (nfa, n) = 
  let candidates = strings(nfa.nfa_alphabet, n) in
  let rec tryAll (l) = 
    match l with
      [] -> []
    | s::ss -> if (nfa_accept(nfa,s)) then
                 s::(tryAll ss)
               else
                 tryAll ss
  in
    tryAll(candidates)

let nfa_printLanguage (nfa,n) = 
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
    printList(nfa_language(nfa,n))


(* Tests *)
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

(* let c = complement (bPlus);;
printLanguage (c,4);;

cross(["hello";"world"],[10;20;30]);;
nfa_hasFinal (abaabStar,["a11";"a21"]);;
nfa_transition (abaabStar,["s"],'a');;
nfa_extendedTransition (abaabStar,["s"],['a';'b';'a']);;
nfa_accept (abaabStar,"a");;
 *)