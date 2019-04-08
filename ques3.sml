(*----------------------------------------------------------------------Accept_Function-------------------------------------------------------------------------------------------------------------------*)

(*type class option*)
datatype class = INT | FIXED | FLOAT | NONE

(* type_of_real tells if string is real number or not and further tells type of real number*)
fun type_of_real( [] , presentState )= presentState
   |type_of_real( l1 as (h::t) ,presentState) = 
   if presentState = 0 then
      if h = #"+" orelse h = #"-" then type_of_real(t,1)
      else if ord(h)>=ord(#"0") andalso ord(h)<=ord(#"9") then type_of_real(t,2)
      else ~1
   else if presentState = 1 then 
      if ord(h)>=ord(#"0") andalso ord(h)<=ord(#"9") then type_of_real(t,2)
      else ~1
   else if presentState = 2 then 
      if ord(h)>=ord(#"0") andalso ord(h)<=ord(#"9") then type_of_real(t,2)
      else if h = #"." then type_of_real(t,3)
      else ~1
   else if presentState = 3 then 
      if ord(h)>=ord(#"0") andalso ord(h)<=ord(#"9") then type_of_real(t,4)
      else ~1
   else if presentState = 4 then 
      if ord(h)>=ord(#"0") andalso ord(h)<=ord(#"9") then type_of_real(t,4)
      else if h = #"E" orelse h = #"e" then type_of_real(t,5)
      else ~1
   else if presentState = 5 then 
      if h = #"+" orelse h = #"-" then type_of_real(t,6)
      else if ord(h)>=ord(#"0") andalso ord(h)<=ord(#"9") then type_of_real(t,7)
      else ~1
   else if presentState = 6 then
      if ord(h)>=ord(#"0") andalso ord(h)<=ord(#"9") then type_of_real(t,7)
      else ~1
   else 
      if ord(h)>=ord(#"0") andalso ord(h)<=ord(#"9") then type_of_real(t,7)
      else ~1

(* accept function-->string->class option*)
fun accept s =
let
   val l1=explode(s);
   val answer=type_of_real(l1,0);
in 
  if answer = ~1 then NONE
  else if answer = 2 then INT
  else if answer = 4 then FIXED
  else if answer = 7 then FLOAT
  else NONE
end
(*--------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------*)

