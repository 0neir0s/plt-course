val x = 1

fun f y = 
let
  val x = y + 1
in
  fn z => x + y + z
end

val x = 3

val g = f 4

val y = 5

val z = g 6
