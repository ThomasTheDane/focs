(* FoCS Spring 2015

   Homework 6 code


   Name: Thomas Nattestad

   Email: thomas.nattestad@students.olin.edu

   Comments: Had a good time, wasn't very hard 

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
 * The type for CFGs
 * 
 *)



type termOrNonTerm =
    T of char
  | N of string


(* CFGs in Chomsky normal form *)

type ccfg = 
  { ccfg_nonterminals : string list;
    ccfg_terminals : char list;
    ccfg_rules : (string * termOrNonTerm list) list;
    ccfg_start : string;
    ccfg_empty : bool
  } 


(* some sample grammars *)

let cgrammar_aab = { ccfg_nonterminals = ["S";"X";"A";"B"];
		    ccfg_terminals = ['a';'b'];
		    ccfg_rules = [ ("S",[N "A";N "A";N "B"]);
				   ("S",[N "A";N "A";N "X";N "B"]);
				   ("X",[N "A";N "A";N "B"]);
				   ("X",[N "A";N "A";N "X";N "B"]);
				   ("A",[T 'a']);
				   ("B",[T 'b'])];
                    ccfg_empty = true;
		    ccfg_start = "S" }

let cgrammar_startend_a = { ccfg_nonterminals = ["S";"X";"Y";"A"];
                           ccfg_terminals = ['a'; 'b'];
                           ccfg_rules = [ ("S",[N "A"; N "A"]);
                                          ("S",[N "A"; N "X"; N "A"]);
                                          ("X",[N "Y"; N "X"]);
                                          ("X",[T 'a']);
                                          ("X",[T 'b']);
                                          ("Y",[T 'a']);
                                          ("Y",[T 'b']);
                                          ("A",[T 'a']) ];
                           ccfg_empty = false;
                           ccfg_start = "S" }
 
let cgrammar_bad = { ccfg_nonterminals = ["S"];
		    ccfg_terminals = ['a';'b'];
		    ccfg_rules = [ ("S",[T 'a';N "A";N "B"]);
				   ("S",[N "A"]);
				   ("S",[]) ];
                    ccfg_empty = true;
		    ccfg_start = "S" }


(* Question 2 *)

let rec found string gen = match gen with 
  | [] -> string == []
  | h::t -> match h with 
    | T s -> if s == (List.hd string) then (found (List.tl string) t) else false 
    | N s -> false

let rec placeAtTip head aList = match aList with 
  | [] -> []
  | h::t -> (head :: h) :: placeAtTip head t

let rec applyRule r string = let (head, body) = r in 
  match string with
    | [] -> []
    | h::t -> if (match h with N a -> a = head | T a -> false) then 
        (body @ t) :: (placeAtTip h (applyRule r t))
      else 
        (placeAtTip h (applyRule r t))

let rec applyAllRules rules string = match rules with 
  | [] -> []
  | h::t -> (applyRule h string) @ applyAllRules t string
  


(* helper function to check if a grammar is Chomsky *)

let check_chomsky grammar = 
  let type1 body = 
    (List.length body >= 2) && 
    (List.for_all (fun x -> match x with N _ -> true | _ -> false)) body in
  let type2 body = match body with [T _] -> true | _ -> false  in
  List.for_all (fun (_,body) -> type1 body || type2 body) grammar.ccfg_rules

(* check if we can generate a goal string *)

let generate grammar goal = 
  let goalL = explode goal  in
  let goal_len = String.length goal  in
  let rec step string = 
    if found goalL string then true
    else if List.length string > goal_len  then false
    else List.exists step (applyAllRules grammar.ccfg_rules string)  in
  if check_chomsky grammar 
    then (if goal = "" then grammar.ccfg_empty
          else step [N grammar.ccfg_start])
  else failwith "Grammar is not in (weak) Chomsky normal form"



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
 *  Compute the language of a Chomsky CFG, restricted to inputs of length <= n
 *   language ccfg n   returns a list of strings generated by ccfg
 *   printLanguage ccfg n   prints the strings generated by ccfg
 *
 *)

let language ccfg n = 
  List.filter (generate ccfg) (strings ccfg.ccfg_terminals n)

let printLanguage ccfg n = 
  List.iter (fun s -> Printf.printf "  %s\n" (if (s="") then "<empty>" else s))
              (language ccfg n);;


found ['a'] [N "a"];;
found ['a';'b';'c'] [T 'a'; T 'b'; T 'c'];;
found ['a'; 'b'] [T 'b'; T 'a'];;

applyRule ("A",[N "X"; N "Y"; N "Z"]) [N "B"];;
applyAllRules cgrammar_aab.ccfg_rules [N "S"; T 'a'; N "S"];;