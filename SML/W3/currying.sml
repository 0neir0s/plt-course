fun sorted3_tupled (x,y,z) = 
  (z >= y) andalso (y >= x)

(* Currying *)

val sorted3 = fn x => fn y => fn z => z >= y andalso y >= x

(* Syntactic sugar - 1 *)
fun sorted3_nicer x y z =  >= y andalso y >= x

(* Syntactic sugar - 2 *)
sorted3 7 8 9
