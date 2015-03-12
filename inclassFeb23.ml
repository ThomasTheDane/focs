let rec filter func xs = match xs with 
	| [] -> []
	| h::t -> if func(h) then h::(filter func t) else filter func t

let rec removeNone xs = filter (fun x -> match x with Some y -> true | None -> false) xs 

let rec map_append f l = 
	match l with 
	  [] -> []
	| x::xs -> (f x)@(map_append f xs)

let flatten l = map_append (fun x -> x) l 

let rec repeat(numTimes, thing) = if numTimes = 0 then [] else thing::repeat(numTimes - 1, thing)

let expand l = map_append (fun x -> repeat(x)) l

let cons a b = a::b;;

let append a b = a@b;;

let rec map_gen comb f l = 
	match l with 
	  [] -> []
	| x::xs -> comb (f x) (map_gen comb f xs)

map_gen append (fun x -> x) [[1;2;3];[4]];;
