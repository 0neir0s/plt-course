fun is_older (d1: int*int*int, d2: int*int*int) = 
let 
  fun order(d: int*int*int) = 
    (#1 d)*10000 + (#2 d)*100 + (#3 d)
in
  order(d1) < order(d2)
end

fun number_in_month (ds: (int*int*int) list, m: int) =
  if null ds
  then 0
  else
    let 
      val dt = hd ds
      val tl_ds = tl ds
      val cnt = if (#2 dt) = m then 1 else 0
    in
      cnt + number_in_month(tl_ds, m)
    end

fun number_in_months (ds: (int*int*int) list, ms: int list) = 
  if null ms
  then 0
  else
   number_in_month(ds, (hd ms)) + number_in_months(ds, (tl ms))

fun dates_in_month (ds: (int*int*int) list, m: int) =
  if null ds
  then []
  else
    let 
      val dt = hd ds
      val tl_ds = tl ds
      val tl_ans = dates_in_month(tl ds, m)
    in
      if (#2 dt) = m
      then dt::tl_ans 
      else tl_ans
    end

fun dates_in_months (ds: (int*int*int) list, ms: int list) = 
  if null ms
  then []
  else
   dates_in_month(ds, (hd ms)) @ dates_in_months(ds, (tl ms))

fun get_nth (ss: string list, n: int) = 
  if n = 1
  then hd ss
  else get_nth (tl ss, n-1)

val months = ["January", "February", "March", "April", "May", "June", "July", "August", "September", "October", "November", "December"]


fun date_to_string (dt: (int*int*int)) = 
  get_nth(months, (#2 dt)) ^ " " ^ Int.toString(#3 dt) ^ ", " ^ Int.toString(#1 dt)

fun number_before_reaching_sum (n: int, ns: int list) = 
let 
  val h = hd ns
  val t = tl ns
  val ht = hd t
in
  if h >= n
  then 0
  else 
    let val cond = (n - h - ht) <= 0
    in
      if cond then 1 else (1 + number_before_reaching_sum(n-h, t))
    end
end

val cml_days = [31,28,31,30,31,30,31,31,30,31,30,31]

fun what_month (day: int) = 
  1 + number_before_reaching_sum(day, cml_days)

fun month_range (from: int, to: int) = 
  if from > to
  then []
  else (what_month from)::(month_range(from+1,to))

fun oldest (ds: (int*int*int) list) = 
  if null ds
  then NONE
  else
    let 
      fun oldest_non_null (ds: (int*int*int) list, curr: (int*int*int)) = 
        if null ds
        then curr
        else
          let 
            val d = hd ds
          in
            oldest_non_null(tl ds, if is_older(curr,d) then curr else d)
          end
    in
      SOME (oldest_non_null(tl ds, hd ds))
    end      
