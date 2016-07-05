(* -.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.
* File Name : deksamenes.sml

* Purpose : Educational

* Creation Date : 04-07-2016

* Last Modified : Tue 05 Jul 2016 01:41:33 PM EEST

* Created By :  Stamatios Anoustis,Artemis Zografou

_._._._._._._._._._._._._._._._._._._._._.*)

fun deksamenes file =
	let
		open IntInf

		fun next_int input = (* scan next int *)
			    Option.valOf (TextIO.scanStream (Int.scan StringCvt.DEC) input)

		val stream = TextIO.openIn file (* open file to read *)

		val N = next_int stream

		val _ = TextIO.inputLine stream

		fun scanner 0 acc = acc
		  | scanner  i acc =
			 let
				 val B = next_int stream
				 val H = next_int stream
				 val W = next_int stream
				 val L = next_int stream
				 val s1 = (fromInt(B),fromInt(W)*fromInt(L))
				 val s2 = (fromInt(B)+fromInt(H),~(fromInt(W)*fromInt(L)))
			 in
						 

				 scanner (i - 1) (s1::s2:: acc) 
			 end  
			val input = rev(scanner (fromInt(N)) [])
			val K = next_int stream

			fun compare ((x,(y:int)) ,(z,(w:int))) =
				if x > z then
					true
				else
					false;
				

			val lstt = ListMergeSort.sort compare input

			fun  loop false lstt ltr rt t t_next i r_prv = (ltr, r_prv, t_next,i)
			  |  loop cont [] ltr rt t t_next i r_prv = (ltr, r_prv, t_next, i)
			  |  loop cont ((h::tl):(int*int)list) ltr rt t t_next i r_prv = 
				let
					val t_next = (#1 h)
					val ltr = ltr + rt*(t_next - t)
					val rt = rt + (#2 h)
					val r_prv = rt - (#2 h)
					val i = i + 1
					val t = t_next
					val cont = (ltr < fromInt(K)) 
				in
					loop cont tl ltr rt t t_next i r_prv
				end

			val res = loop true lstt 0 0 0 0 0 0

			val h = Real.fromLargeInt((#1 res) - fromInt(K))/Real.fromLargeInt(#2 res)
			val overflow = (#4 res) >= (2*IntInf.fromInt(N))
			open Real
		in
		  if overflow then
			Real.fromInt(~1)
		  else
		  	Real.fromLargeInt(#3 res) - h	
		  end


