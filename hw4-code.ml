(* FoCS Spring 2015

   Homework 4 code


   Name: Thomas Nattestad

   Email: thomas.nattestad@students.olin.edu

   Comments: Really liked it a lot. Did the binary tree in postorder

 *)





(*
 *
 *  QUESTION 1 
 * 
 *
 *)


let rec last l = 
  match l with
    [] -> None
  | [x] -> Some(x)
  | x::xs -> last(xs)


let predicate_opt p = fun x -> if p x then Some(x) else None

let map_opt f = fun x -> match x with Some x -> Some(f x) | _ -> None

let comb_opt f = fun x y -> match x, y with Some x, Some y -> Some(f x y) | _ -> None

let default v = fun x -> match x with Some x -> x | _ -> v

let compose_opt f g = fun x -> 
  match f x with 
    Some a -> match g a with Some b -> Some b | _ -> None
    | _ -> None

let at_least n p xs = (List.fold_right (fun x y -> if p x then 1 + y else y) xs 0) > n

(* Assuming positive integers *)
let max_list xs = match List.fold_right (fun x y -> if x > y then x else y) xs 0 with
  | 0 -> None
  | a -> Some(a)

let map_funs fs x = List.fold_right (fun a b -> (a (x)) :: b) (fs) []

let map_cross fs xs = List.fold_right (fun a b -> (map_funs fs a) @ b) xs []




(*
 * 
 * QUESTION 3
 * 
 * 
 *)

let rec suffixes xs = match xs with
  | [] -> [[]]
  | h::t -> [xs] @ suffixes t

(* let suffixes xs = List.fold_right (fun a b -> ([a @ match b with []->[] | h::t -> h] @ b)) xs [[]] *)

let prefixes xs = failwith "not implemented"

let inject a xs = failwith "not implemented"

let perms xs = failwith "not implemented"




(*
 * 
 * QUESTION 4
 * 
 * 
 *)


type 'a bintree = 
    EmptyTree 
  | Node of 'a * 'a bintree * 'a bintree

let sample = 
  Node (10, Node (3, Node (7, EmptyTree, EmptyTree),
                     Node (5, EmptyTree, EmptyTree)),
            Node (6, Node (99, EmptyTree, 
                               Node (66, EmptyTree, EmptyTree)),
                     EmptyTree))
                                 
let print_tree t = 
  let print_typ_tree f t = 
    let emptyString n = String.make n ' '  in
    let ljustify n s = s ^ (emptyString (n - (String.length s)))  in
    let height p = List.length p  in
    let width p = List.fold_right (fun s m -> max (String.length s) m) p 0  in
    let rec copy n x = 
      if (n <= 0)
        then []
      else x :: copy (n - 1) x  in
    let empty h w = copy h (emptyString w)  in
    let above p q = 
      let w = max (width p) (width q)
      in (List.map (ljustify w) p) @ (List.map (ljustify w) q)  in
    let beside p q = 
      let h = max (height p) (height q)  in
      let heighten h p = above p (empty (h - List.length p) (width p))
      in List.map2 (^) (heighten h p) (heighten h q)  in
    let string_picture p = (String.concat "\n" p)^"\n"  in
    let print_picture p = Printf.printf "%s" (string_picture p)  in
    let rec picture_tree f t = 
      match t with
        EmptyTree -> [" "]
      | Node (v,EmptyTree,EmptyTree) -> [f v]
      | Node (v,EmptyTree,r) -> above [f v]
            (above ["---|"]
               (beside ["   "] (picture_tree f r)))
      | Node (v,l,EmptyTree) -> above [f v]
            (above ["|"] 
               (picture_tree f l))
      | Node (v,l,r) -> let sub_l = picture_tree f l in
        let sub_r = picture_tree f r
        in above [f v]
          (above ["|"^(String.make (2 + width sub_l) '-')^"|"]
             (beside sub_l (beside ["   "] sub_r)))
    in print_picture (picture_tree f t) in
  print_typ_tree string_of_int t


let rec mapT f t = 
  match t with
    EmptyTree -> EmptyTree
  | Node (v,l,r) -> Node (f v, mapT f l, mapT f r)

let rec foldT f t b = 
  match t with
    EmptyTree -> b
  | Node (v,l,r) -> f v (foldT f l b) (foldT f r b)

let size t = foldT (fun v l r -> 1 + l + r) t 0
let sum t = foldT (fun v l r -> v + l + r) t 0

let max(a, b) = if a > b then a else b

let rec height t = match t with 
  | EmptyTree -> 0
  | Node (v,l,r) -> 1 + max(height l, height r)

let height' t = foldT (fun v l r -> 1 + max(l, r)) t 0

let rec fringe t = match t with 
  | EmptyTree -> []
  | Node (v,l,r) -> match l, r with 
    | EmptyTree, EmptyTree -> [v]
    | EmptyTree, Node(v2, l2, r2) -> fringe r
    | Node(v1, l1, r1), EmptyTree -> fringe l
    | Node(v1, l1, r1), Node(v2, l2, r2) -> (fringe l) @ (fringe r)

let fringe' t = foldT 
  (fun v l r -> match l,r with [], [] -> [v] | _ -> l @ r) t [] 

let rec inorder t = match t with 
  | EmptyTree -> []
  | Node (v,l,r) -> (inorder l) @ [v] @ (inorder r)

let rec preorder t = match t with 
  | EmptyTree -> []
  | Node (v,l,r) -> v :: ((inorder l) @ (inorder r))

let rec postorder t = match t with 
  | EmptyTree -> []
  | Node (v,l,r) -> (postorder l) @ (postorder r) @ [v]

let rec listSize(l) = match l with [] -> 0 | h::t -> 1 + listSize(t);;

let rec listAfter l i = match l with 
  | [] -> []
  | h::t -> if i-1 > 0 then listAfter t (i-1) else t

let rec listBefore l i = match l with 
  | [] -> []
  | h::t -> if i > 0 then h::listBefore t (i-1) else []

let rec itemAt l i = match l with 
  | [] -> failwith "index out"
  | h::t -> if i == 0 then h else itemAt t (i-1)

let split l = let splitPoint = ((listSize(l)) / 2) in ((itemAt l splitPoint) ,(listBefore l splitPoint), (listAfter l (splitPoint+1)))

let rec make_tree xs = match xs with 
  | [] -> EmptyTree
  | _ -> let (treeVal, firstHalf, secondHalf) = split xs in Node(treeVal, make_tree firstHalf, make_tree secondHalf);;

(* print_tree (make_tree [1;2;3;4;5;6;7;8]);; *)
