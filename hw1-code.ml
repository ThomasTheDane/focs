(* FoCS Spring 2015

   Homework 1 code


   Name: Thomas Nattestad

   Email: thomas.nattestad@students.olin.edu

   Comments: Really fun problem set. I wasn't really sure what the syntax style conventions were so I just tried to come up with some that made sense to me, would love to hear more about more official styles of code organization. 
      The last problem 4c crazy hard since we hadn't talked about Constant = True | False syntax. Worked with Brendan C and Josh L to solve it but worked independently on all others
 *)



(* 
 *  Question 1
 *)

let rec append(xs, ys) = match xs with
  | [] -> ys
  | h :: t -> h :: append(t, ys);;

let rec flatten(xs) = match xs with 
  | [] -> [] 
  | h::t -> append(h, flatten(t));;

let rec double (xs) = match xs with
  | [] -> []
  | h::t -> (h*2) :: double(t);;
  

let rec last (xs) = match xs with
  | [] -> None
  | [a] -> Some a
  | h::t -> last(t);;

(*
 *  Question 2 
 *)

let rec setIn (elt,set) = match set with 
  | [] -> false
  | h::t -> 
      if h = elt then true 
      else setIn(elt, t);;

let rec setSub (set1,set2) = match set1 with 
  | [] -> true
  | h::t ->
      if setIn(h, set2) then setSub(t, set2) 
      else false;;

let setEqual (set1,set2) = 
  if setSub(set1, set2) && setSub(set2, set1) then true else false;;

let rec getUniqueSet(set) = match set with
  | [] -> []
  | h::t -> 
      if setIn(h, t) then getUniqueSet(t)
      else h :: getUniqueSet(t);;

let setUnion (set1,set2) = getUniqueSet(append(set1, set2))
  
let rec setInter (set1,set2) = match set1 with 
  | [] -> []
  | h :: t -> 
      if setIn(h, set2) then h :: setInter(t, set2)
      else setInter(t, set2);;

let rec listSize(l) = match l with [] -> 0 | h::t -> 1 + listSize(t);;

let rec setSize (set) = listSize(getUniqueSet(set));;

(* 
 *  Question 3
 *)

type rat = {num: int; den: int}

let half = {num=1; den=2}
let third = {num=1; den=3}
let fourth = {num=1; den=4}

let floatR (r) = float(r.num) /. float(r.den)

let rec gcd(a, b) = if b = 0 then a else gcd(b, (a mod b));;

let handleNegs(r) = 
  if r.num < 0 && r.den < 0 then {num = -r.num ; den = -r.den}
  else if r.num > 0 && r.den < 0 then {num = -r.num ; den = -r.den}
  else r

let simplify (r) = handleNegs({num = r.num / gcd(abs(r.num), abs(r.den)); 
                               den = r.den / gcd(abs(r.num), abs(r.den))}
                             );;

let addR (r1,r2) = simplify({num = (r1.num * r2.den) + (r2.num * r1.den); den = r1.den * r2.den})
  

let multR (r1,r2) = simplify({num = r1.num * r2.num; den = r1.den * r2.den})

type number = I of int
            | R of rat
            | F of float

let add (n1,n2) = match (n1,n2) with 
    | (I i, I j) -> I (i+j)
    | (I i, F f) -> F (float(i) +. f)
    | (I i, R r) -> R (addR({num = i; den = 1}, r))
    | (F f, I i) -> F (float(i) +. f)
    | (F f, F g) -> F (f +. g)
    | (F f, R r) -> F (f +. floatR(r))
    | (R r, I i) -> R (addR({num = i; den = 1}, r))
    | (R r, F f) -> F (f +. floatR(r))
    | (R r, R s) -> R (addR(r, s))

(* 
 *  Optional question
 *)


type bConst = True | False

type bExpr = Constant of bConst
           | Variable of string
           | And of bExpr * bExpr
           | Or of bExpr * bExpr
           | Not of bExpr

let sample1 = And(Not(Variable "a"),Not(Variable "b"))

let sample2 = Or(Not(Variable "a"),And(Variable "b",Constant(True)))

let sample3 = And(Variable "a", Not(Variable "a"))

(* Awww yeee, using that function from a long time ago to get rid of duplicates *)
let rec vars (bexpr) = getUniqueSet(
  match bexpr with 
    | Constant c -> []
    | Variable v -> [v]
    | And (a1, a2) -> append(vars(a1), vars(a2))
    | Or (o1, o2) -> append(vars(o1), vars(o2))
    | Not n -> vars(n);;
  )

let rec subst (bexpr,var,sub) = match bexpr with 
  | Constant c ->  Constant c
  | Variable v -> if v = var then sub else Variable v 
  | And (a1, a2) -> And (subst(a1, var, sub), subst(a2, var, sub))
  | Or (o1, o2) -> Or (subst(o1, var, sub), subst(o2, var, sub))
  | Not n -> Not (subst(n, var, sub))

let evalConst(const) = match const with True -> true | False -> false

let rec evalBooleans (bexpr) = match bexpr with
  | Constant True -> true
  | Constant False -> false
  | And (a1, a2) -> evalBooleans(a1) && evalBooleans(a2)
  | Or (o1, o2) -> evalBooleans(o1) || evalBooleans(o2)
  | Not n -> not(evalBooleans(n))
  | _ -> false

let rec eval (bexpr) =
  if vars(bexpr) <> [] then None
  else if evalBooleans(bexpr) then Some True else Some False

