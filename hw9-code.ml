(* FoCS Spring 2015

   Homework 9 code


   Name: Thomas Nattestad

   Email: thomas.nattestad@students.olin.du

   Comments: Really fun homework but more explanation on how fby worked would have made it go faster

 *)


  
(* The underlying implementation for streams 
 *
 * You don't need to know anything about this code -- you will
 * use functions fby, head, and tail described below instead.
 *
 * But if you're curious, a stream is a pair of an element and a 
 * "promise" to compute the rest of the stream.
 * 
 * That "promise" is represented as a function
 *
 * The implementation memoizes that function: once the function is
 * called once, it remembers its result, and subsequent calls to the
 * function directly return the result without executing the body of
 * the function
 *
 *)
  
module AbsStream : 
    sig
      type 'a stream 
      val mk : 'a -> (unit -> 'a stream) -> 'a stream 
      val unmk1 : 'a stream -> 'a 
      val unmk2 : 'a stream -> 'a stream 
    end = 
  struct
    
    type 'a stream = R of 'a * (unit -> 'a stream)
	  
    let memoize f = 
      let memoized = ref None in
      let new_f () = 
	match !memoized with
	| None -> let result = f () in memoized := Some result; result
	| Some v -> v   in
      new_f
	
    let mk h t = R (h, memoize t) 
    let unmk1 s = let R (h,t) = s in h
    let unmk2 s = let R (h,t) = s in t ()
  end


(*
 * These are the stream functions you will want to use
 *
 *)

type 'a stream = 'a AbsStream.stream
let head : 'a stream -> 'a = AbsStream.unmk1
let tail : 'a stream -> 'a stream = AbsStream.unmk2
let fby : 'a -> (unit -> 'a stream) -> 'a stream = AbsStream.mk






(* 
 * list n s 
 * nth n s
 * list2 n m s
 *
 *
 * list : returns the list of the first 'n' elements of 's'
 * nth : returns the 'n'th element of stream 's'
 * list2 : returns the list of lists of the first 'm' elements
 *           of each of the first 'n' streams of 's'
 *
 *)


let rec list n s = 
  if n <= 0 then []
  else (head s) :: (list (n-1) (tail s))

let rec list2 n m s = 
  List.map (list m) (list n s)

let rec nth n s = 
  if n <= 0 then head s
  else nth (n - 1) (tail s)


(* 
 * const k : returns the constant stream of 'k's
 * from k : returns the stream of integers starting at 'k'
 * 
 *)

let rec const k = fby k (fun () -> const k)

let rec from k = fby k (fun () -> from (k+1))

let nats = from 0

(*
 * map f s : returns the stream obtained by applying 'f' 
 *             to every element of 's' 
 * filter p s : returns the stream of elements of 's' for which 'p' is true
 *
 *)

let rec map f s = fby (f (head s)) (fun () -> (map f (tail s)))

let rec map2 f s1 s2 = fby (f (head s1) (head s2)) (fun () -> map2 f (tail s1) (tail s2))

let plus1 s = map (fun x -> x + 1) s

let evens = map (fun x -> 2 * x) nats

let odds = plus1 evens

let squares = map (fun x -> x * x) nats

let rec filter p s = 
  if (p (head s)) 
  then fby (head s) (fun () -> filter p (tail s))
  else filter p (tail s)

let even s = filter (fun x -> (x mod 2 = 0)) s



(*
 * The Sieve of Eratosthenes
 *
 *)


let not_divisible_by n k = not (k mod n = 0)

let rec sieve s = 
   fby (head s) 
       (fun () -> sieve (filter (not_divisible_by (head s)) (tail s)))

let primes = sieve (from 2)



(* 
 * QUESTION 1 
 * 
 *)

let rec rec_stream s comb prev = 
  let h = comb (head s) prev in 
    fby (h) (fun () -> rec_stream (tail s) comb h)

let scale n s = map (fun x -> n * x) s

let rec zip s1 s2 = map2 (fun x y -> (x, y)) s1 s2 

let add s1 s2 = map2 (fun x y -> (x + y)) s1 s2

let rec merge s1 s2 = (fby (head s1) (fun () -> (fby (head s2) (fun () -> (merge (tail s1) (tail s2))))))

let rec psums_helper s sum = fby ((head s) + sum) (fun () -> (psums_helper (tail s) ((head s) + sum)))

let rec psums s = psums_helper s (head s)

let rec running_max_helper s max = 
  if (head s) > max then 
    fby (head s) (fun () -> (running_max_helper (tail s) (head s)))
  else
    fby (max) (fun () -> (running_max_helper (tail s) (max)))

let rec running_max s = running_max_helper s (head s)


(*
 * QUESTION 2
 * 
 *)

let scalef n s = map (fun x -> n *. x) s

let rec arctan_helper z level sum = 
  let expo = (level *. 2.0) -. 1.0 in 
    if (int_of_float level) mod 2 = 0 then 
      let newTerm = -.1.0*.(((z**expo) /. expo)) in 
        fby ((newTerm) +. sum) (fun () -> (arctan_helper z (level +. 1.0) (sum +. newTerm)))
    else
      let newTerm = (((z**expo) /. expo)) in 
        fby ((newTerm) +. sum) (fun () -> (arctan_helper z (level +. 1.0) (sum +. newTerm)))

let rec arctan z = arctan_helper z 1. 0.

let pi () = map2 (fun x y -> (16. *. x) -. (4. *. y)) (arctan (1./.5.)) (arctan (1. /. 239.))
    
let rec newton f df guess = let newGuess = guess -. ((f guess) /. (df guess)) in 
  fby (newGuess) (fun () -> (newton f df newGuess))

let rec derivative_helper f x level = fby ( ((f (x +. (1. /. level))) -. (f x)) /. (1./.level) ) (fun () -> (derivative_helper f x (level+.1.)))

let derivative f x = derivative_helper f x 1.0

let rec limit_helper mx eps s lastVal = 
  if mx > 0 then 
    if abs_float((head s) -. lastVal) < eps then
      Some(head s)
    else
      limit_helper (mx - 1) eps (tail s) (head s) 
  else
    None

let limit mx eps s = limit_helper mx eps s ((head s) -. ((float_of_int mx) *. 2.))


(* 
 * QUESTION 3 
 * 
 *)

let rec makeStreamPair value s = (fby ((value,(head s))) (fun () -> (makeStreamPair value (tail s))));;

let rec table s1 s2 = fby (makeStreamPair (head s1) s2) (fun () -> table (tail s1) s2)

let rec compose_stripe s n = 
  if n >= 0 then 
    nth n (head s) :: (compose_stripe (tail s) (n-1))
  else
    []

let rec stripes_helper s n = fby (compose_stripe s n) (fun () -> (stripes_helper (s) (n+1)))

let rec stripes s = stripes_helper s 0

let rec flatten s = match (head s) with
  | [] -> flatten (tail s)
  | h::t -> fby (h) (fun () -> flatten (fby (t) (fun () -> (tail s))))

let pairs s1 s2 = flatten (stripes (table s1 s2))












