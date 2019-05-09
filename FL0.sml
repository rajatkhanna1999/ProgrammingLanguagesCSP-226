(*-------------------------------------------------------------------------FL0 Assignment--------------------------------------------------------------------------------------------------------------*)
(*--------------------------------------------------------------------------Rajat Khanna---------------------------------------------------------------------------------------------------------------*)
(*--------------------------------------------------------------------------2017ucs0050----------------------------------------------------------------------------------------------------------------*)
(*-----------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------*)

exception error
datatype  fl0 = T | F | Z |x|y|z | IZ of  fl0 | P of  fl0 | S of  fl0 | ITE of  fl0 *  fl0 *  fl0 | GTZ of  fl0 | lambda of  fl0 * fl0  | application of fl0 * fl0 | REC of  fl0



 fun evaluate(Z) = Z |
    evaluate(T) = T |
    evaluate(F) = F |

    (*predecessor function to check predecessor statements*)
    evaluate(P(t)) =
	let fun predecessor(P(F)) = raise error |	 
    		predecessor(P(T)) = raise error |
		predecessor(P(Z)) = P(Z) |	
    		predecessor(P(S(t1))) = evaluate(t1) |
    		predecessor(P(t1)) = if evaluate(t1)=t1 then P(x) else evaluate(P(evaluate(t1)))
	in	predecessor(P(t))
	end			|

   (*successor function to check successor statements*)
    evaluate(S(t)) = 
    let fun	successor(S(T)) = raise error |
   		successor(S(F)) = raise error |  
		successor(S(Z)) = S(Z) | 
    		successor(S(P(t1))) = evaluate(t1) |
    		successor(S(t1)) = 
			let val temp = evaluate(t1)
       			in
			if temp=t1 then S(t1) else evaluate(S(temp))
			end	
	in	successor(S(t))            
	end				|	


    evaluate(IZ(t1)) = if evaluate(t1)=Z then T else F |

    evaluate(ITE(t1,t2,t3)) = if evaluate(t1)=T then evaluate(t2) else if evaluate(t1)=F then evaluate(t3) else ITE(t1,t2,t3) |		

    (*lambda abstractions to check lambda expression*)
    evaluate(lambda(x1,Y))=lambda(x1,evaluate(Y)) |

    (*Beta reductions*)
    evaluate(application(lambda(x1,L),M))=
		let fun substitute(x1,application(y1,y2),z1)=application(substitute(x1,y1,z1),substitute(x1,y2,z1))  |
		        substitute(x1,lambda(y1,y2),z1)=lambda(y1,substitute(x1,y2,z1)) |
			substitute(x1,S(Z),z1)=S(Z) |
		        substitute(x1,P(Z),z1)=P(Z) |
		        substitute(x1,P(t0),z1)=P(substitute(x1,t0,z1)) |
		        substitute(x1,S(t0),z1)=S(substitute(x1,t0,z1)) |
		        substitute(x1,IZ(t0),z1)=IZ(substitute(x1,t0,z1)) |
		        substitute(x1,GTZ(t0),z1)=GTZ(substitute(x1,t0,z1)) |
		        substitute(x1,ITE(t1,t2,t3),z1)=ITE(substitute(x1,t1,z1),substitute(x1,t2,z1),substitute(x1,t3,z1)) |
		        substitute(x1,y1,z1)= if x1=y1 then z1	else y1		

		in   evaluate(substitute(x1,evaluate(L),evaluate(M)))
		end			|

    (*Application that is (L M) *)
    evaluate(application(application(x1,y1),z1))=
		let val X=evaluate(application(x1,y1))
		    val Y=evaluate(z1)	
		in evaluate(application(X,Y))
		end  			|

    evaluate(GTZ(t)) =   
    let fun greaterthanzero(GTZ(Z)) = F |
    	    greaterthanzero(GTZ(S(Z))) = T |
            greaterthanzero(GTZ(P(Z))) = F |
	    greaterthanzero(GTZ(S(P(t1)))) = evaluate(GTZ(t1)) |
	    greaterthanzero(GTZ(P(S(t1)))) = evaluate(GTZ(t1)) |
            greaterthanzero(GTZ(P(t1))) =  if evaluate(t1)=t1 then F else evaluate(GTZ(P(evaluate(t1))))   |
            greaterthanzero(GTZ(S(t1))) = if evaluate(t1)=t1 then T else evaluate(GTZ(S(evaluate(t1)))) 	    
    in  greaterthanzero(GTZ(t))
    end				|

    evaluate(x1)=x1

(*------------------------------------------------------------------The End of Semester-Goodbye SAK ---------------------------------------------------------------------------------------------------*)
