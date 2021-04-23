(* if you use this function to compare two strings (returns true if the same
   string), then you avoid several of the functions in problem 1 having
   polymorphic types that may be confusing *)
fun same_string(s1 : string, s2 : string) =
    s1 = s2

(* put your solutions for problem 1 here *)
fun all_except_option(s1, xs) =
let fun checker(xs) = case xs of 
                           [] => []
                         | x::xs' => 
                             if same_string(s1,x)
                             then xs'
                             else x::checker(xs')
  val answer = checker(xs)
in
  if answer = xs then NONE else SOME answer
end

fun get_substitutions1(xss, s) = 
case xss of 
     [] => []
   | xs::xss' =>
       let 
         val answer = all_except_option(s,xs) 
         val subset =  get_substitutions1(xss',s)
         in case answer of 
                 NONE => subset
               | SOME value => value @ subset
       end

fun get_substitutions2(xss,s) =
let fun helper(xss, acc) = 
case xss of 
     [] => acc
   | xs :: xss' =>
       case all_except_option(s,xs) of
                        NONE => helper(xss',acc)
                      | SOME value => helper(xss', acc @ value)
in helper(xss, [])
end

fun similar_names(xss, {first=f, middle=m, last=l}) = 
let fun helper(xs) = 
case xs of 
     [] => []
   | x :: xs' => {first=x, middle=m, last=l} :: helper(xs')
in
  {first=f, middle=m, last=l} :: helper( get_substitutions2(xss, f))
end

(* you may assume that Num is always used with values 2, 3, ..., 10
   though it will not really come up *)
datatype suit = Clubs | Diamonds | Hearts | Spades
datatype rank = Jack | Queen | King | Ace | Num of int 
type card = suit * rank

datatype color = Red | Black
datatype move = Discard of card | Draw 

exception IllegalMove

(* put your solutions for problem 2 here *)

fun card_color (s,r) = 
  case s of 
       Clubs => Black
     | Spades => Black
     | _ => Red

fun card_value (s,r) =
  case r of 
       Ace => 11
     | Num i => i
     | _ => 10

fun remove_card (cs, c, e) = 
let fun helper (cs) = 
case cs of 
     [] => []
   | c'::cs' =>
       if c' = c then cs' else c' :: helper(cs')
    val answer = helper(cs)
in
  if answer=cs then raise e else answer
end

fun all_same_color (cs) =
  case cs of 
       [] => true
     | c :: [] => true
     | c :: (c' :: cs') => (card_color c = card_color c') andalso all_same_color
     (c' :: cs') 

fun sum_cards (cs) =
let fun helper (cs, acc) = 
case cs of 
     [] => acc
   | c :: cs' => helper(cs', acc + card_value(c))
in
  helper (cs, 0)
end

fun score (cs, goal) = 
let 
  val s = sum_cards (cs)
  val ps = if s>goal then 3*(s-goal) else (goal-s)
in
  if all_same_color (cs) then (ps div 2) else ps
end

fun officiate (cs, ms, goal) = 
let 
  fun helper (cs, ms, hs) =
  case ms of
       [] => score (hs, goal)
     | (Discard c) :: ms' => 
         helper (cs, ms', remove_card (hs, c, IllegalMove))
     | Draw :: ms' =>
         case cs of 
              [] => score (hs, goal)
            | c :: cs' => 
                let 
                  val hs' = c :: hs
                  val shc = sum_cards (hs')
                in case (shc>goal) of
                        true => score (hs', goal)
                      | false => helper (cs', ms', hs')
                end
in
 helper (cs, ms, [])
end 
