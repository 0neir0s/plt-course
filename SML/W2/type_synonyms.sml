datatype suit = Club | Diamond | Heart | Spade

datatype rank = Jack | Queen | King | Ace | Num of int

type card = suit * rank

type name_record = { student_id : int option,
                     student_name: string }

fun is_q_s (c: card) = 
  #1 c = Spade andalso #2 c = Queen
