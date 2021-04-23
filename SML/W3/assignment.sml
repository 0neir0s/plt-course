(* Coursera Programming Languages, Homework 3, Provided Code *)

exception NoAnswer

datatype pattern = Wildcard
		 | Variable of string
		 | UnitP
		 | ConstP of int
		 | TupleP of pattern list
		 | ConstructorP of string * pattern

datatype valu = Const of int
	      | Unit
	      | Tuple of valu list
	      | Constructor of string * valu

fun g f1 f2 p =
    let 
	val r = g f1 f2 
    in
	case p of
	    Wildcard          => f1 ()
	  | Variable x        => f2 x
	  | TupleP ps         => List.foldl (fn (p,i) => (r p) + i) 0 ps
	  | ConstructorP(_,p) => r p
	  | _                 => 0
    end

val only_capitals = List.filter (fn s => Char.isUpper (String.sub (s,0))) 

val longest_string1  = List.foldl (fn (s,acc) => if String.size s > String.size acc
                                          then s else acc) ""
val longest_string2 = List.foldl (fn (s,acc) => if String.size s >= String.size acc
                                          then s else acc) ""
fun longest_string_helper f ss = List.foldl (fn (s,acc) => if f(String.size s,
  String.size acc)
                                                             then s else acc)
                                                             "" ss

val longest_string3 = longest_string_helper (fn (a:int,b:int) => a>b) 
val longest_string4 = longest_string_helper (fn (a:int,b:int) => a>=b)

val longest_capitalized = longest_string1 o only_capitals

val rev_string = String.implode o List.rev o String.explode

fun first_answer f xs = 
  case xs of
       [] => raise NoAnswer 
     | x::xs' => case f x of
                      NONE => first_answer f xs'
                    | SOME v => v

fun all_answers f xs =
let 
  fun appender (SOME acc, lst) = SOME (acc@lst)
  fun helper (acc, xs) = 
   case xs of
       [] => acc
     | x::xs' => case f x of
                      NONE => NONE
                    | SOME lst => helper (appender (acc, lst), xs')
in
  helper (SOME [], xs)
end

val count_wildcards = g (fn () => 1) (fn (_) => 0)

val count_wild_and_variable_lengths = g (fn () => 1) (String.size)

fun count_some_var (s,p) = g (fn () => 0) (fn s' => if s=s' then 1 else 0) p

fun check_pat p =
    let 
      fun var_list p =
	case p of
	    Variable x        => [x]
	  | TupleP ps         => List.foldl (fn (p,i) => (var_list p) @ i) [] ps
	  | ConstructorP(_,p) => var_list p
	  | _                 => []
      fun no_duplicate ss =
       case ss of 
            [] => true
          | s::ss' => not (List.exists (fn i => i=s) ss') andalso no_duplicate ss'
    in
      no_duplicate (var_list p)
    end

fun match (v, p) =   
  case (v,p) of 
       (_, Wildcard) => SOME []
     | (v',Variable s) => SOME [(s,v')]
     | (Unit, UnitP) => SOME []
     | (Const i, ConstP j) => if i=j then SOME [] else NONE
     | (Tuple vs, TupleP ps) => if (length vs) <> (length ps)
                                then NONE
                                else all_answers match (ListPair.zip (vs,ps))
     | (Constructor (s1,v'), ConstructorP (s2, p')) => if s1<>s2 
                                                      then NONE
                                                      else match (v',p')
     | _ => NONE


fun first_match v ps = 
  SOME (first_answer (fn p => match (v,p)) ps)
  handle NoAnswer => NONE 
