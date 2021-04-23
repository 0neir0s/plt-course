datatype exp = Constant of int
             | Negate of exp
             | Add of exp*exp
             | Multiply of exp*exp

fun true_of_all_constants(f, e) = 
  case e of 
       Constant i => f i
     | Negate e1 => true_of_all_constants(f, e1)
     | Add(e1,e2) => true_of_all_constants(f, e1) andalso
     true_of_all_constants(f, e2)
     | Multiply(e1,e2) => true_of_all_constants(f, e1) andalso
     true_of_all_constants(f, e2)

fun all_even e  = true_of_all_constants ((fn x => x mod 2 = 0), e)
