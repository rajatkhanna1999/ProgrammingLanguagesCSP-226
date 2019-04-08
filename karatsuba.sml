(*
Rajat Khanna
2017UCS0050
*)
local
exception NotUnsignedIntegers;
(*fun1 -- To check Validity*)
fun validate [] = true
      | validate (h::t) = if (ord(h)>=48 andalso ord(h)<=57 ) then validate(t)
			  else false

(*fun2 -- To append at front,at end and remove leading zeros--helper functions*)
      fun appfront (atl1,0)=atl1
    	| appfront (atl1,m)=appfront((#"0")::atl1,m-1)

      fun appback(l,0) = l
      	| appback(l,m)=appback(l@[#"0"],m-1)

      fun removeLeadingZeros([])=[]
      	| removeLeadingZeros(l as h::t)=
      	  if h=(#"0")then removeLeadingZeros(t)
	  else l

(*fun3 -- To add two lists of any length*)
   fun add1(atl1,atl2)=
	  let  fun add (c,[],[]) = [c]
     	       	 | add (c,(h1::t1),(h2::t2)) =
	         chr(((ord(h1)+ord(h2)+ord(c)-ord(#"0")-ord(#"0")-ord(#"0")) mod 10)+48)::add(chr(((ord(h1)+ord(h2)+ord(c)-ord(#"0")-ord(#"0")-ord(#"0")) div 10)+48),t1,t2)
	  in if (List.length(atl1)<List.length(atl2)) then List.rev(add(#"0",List.rev(appfront(atl1,List.length(atl2)-List.length(atl1))),List.rev(atl2)))
	     else  List.rev(add(#"0",List.rev(atl1),List.rev(appfront(atl2,List.length(atl1)-List.length(atl2)))))
	  end

(*fun4 -- To Judge which number is big and which is small*)
fun sgn (lll1,lll2)=
    let fun sgn([],[])=true
      	  | sgn((h1::t1),(h2::t2))=
	     if(ord(h1)<ord(h2))then false
	     else
	       if(ord(h1)>ord(h2))then true
	       else sgn(t1,t2)		 
    in if(List.length(lll1)<List.length(lll2))then sgn(appfront(lll1,List.length(lll2)-List.length(lll1)),lll2)
       else sgn(lll1,appfront(lll2,List.length(lll1)-List.length(lll2)))
    end

(*fun5 --Two subtract two lists , returns positive value*)
     fun sub1(sl1,sl2)=
     	  let fun sub(c,[],[])=[]
	        | sub(c,(h1::t1),(h2::t2))=
	        if((ord(h1)-ord(h2)-ord(c)+ord(#"0"))<0)then chr(ord(h1)-ord(h2)-ord(c)+ord(#"0")+10+48)::sub(#"1",t1,t2)
	        else chr(ord(h1)-ord(h2)-ord(c)+ord(#"0")+48)::sub(#"0",t1,t2)
	  in if (List.length(sl1)<List.length(sl2)) then
	     	 if (sgn(appfront(sl1,List.length(sl2)-List.length(sl1)),sl2)=true)then List.rev(sub(#"0",List.rev(appfront(sl1,List.length(sl2)-List.length(sl1))),List.rev(sl2)))
	         else  List.rev(sub(#"0",List.rev(sl2),List.rev(appfront(sl1,List.length(sl2)-List.length(sl1)))))
	     else
		if (sgn(sl1,appfront(sl2,List.length(sl1)-List.length(sl2)))=true)then List.rev(sub(#"0",List.rev(sl1),List.rev(appfront(sl2,List.length(sl1)-List.length(sl2)))))
	        else  List.rev(sub(#"0",List.rev(appfront(sl2,List.length(sl1)-List.length(sl2))),List.rev(sl1)))
	  end
(*fun6-- karatsuba to multiply two numbers*)
  fun karatsuba ([x1],[x2]) = [chr((((ord(x1)-ord(#"0"))*(ord(x2)-ord(#"0"))div 10)+48)),chr((((ord(x1)-ord(#"0"))*(ord(x2)-ord(#"0"))mod 10)+48))]
        | karatsuba ((l1 as h1::t1),(l2 as h2::t2))=
	let val m=(List.length(l1)+1)div 2; 
	   val (x1,x0)=(List.take(l1,length(l1)-m),List.drop(l1,length(l1)-m));
	   val (y1,y0)=(List.take(l2,length(l1)-m),List.drop(l2,length(l1)-m));
	   val z0=karatsuba(x0,y0);
	   val z2=karatsuba(x1,y1);
	   val q1=sub1(x0,x1);
	   val q2=sub1(y1,y0);
	   val zsum=add1(z0,z2);

	in   if ((sgn(x0,x1)=true andalso sgn(y1,y0)=true) orelse (sgn(x0,x1)=false andalso sgn(y1,y0)=false))then
	      	  if (List.length(q1)<List.length(q2)) then
	      	 add1(add1(appback(z2,2*((List.length(l1)+1)div 2)),z0),appback(add1(zsum,karatsuba(appfront(q1,List.length(q2)-List.length(q1)),q2)),(List.length(l1)+1)div 2))
	          else
		 add1(add1(appback(z2,2*((List.length(l1)+1)div 2)),z0),appback(add1(zsum,karatsuba(q1,appfront(q2,List.length(q1)-List.length(q2)))),(List.length(l1)+1)div 2))
	      else
	          if (List.length(q1)<List.length(q2)) then
			add1(add1(appback(z2,2*((List.length(l1)+1)div 2)),z0),appback(sub1(zsum,karatsuba(appfront(q1,List.length(q2)-List.length(q1)),q2)),(List.length(l1)+1)div 2))
	      	 else
	     	        add1(add1(appback(z2,2*((List.length(l1)+1)div 2)),z0),appback(sub1(zsum,karatsuba(q1,appfront(q2,List.length(q1)-List.length(q2)))),(List.length(l1)+1)div 2))
	     
	end

(*fun7 -- main function*)
in
fun mainprogram(s1,s2)=
 let val l1=explode(s1);
       val l2=explode(s2);
in if(validate(l1)=false orelse validate(l2)=false) then raise NotUnsignedIntegers
   else implode(removeLeadingZeros(karatsuba(l1,l2)))
end
end

