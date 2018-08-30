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

datatype typ = Anything
	     | UnitT
	     | IntT
	     | TupleT of typ list
	     | Datatype of string


fun shortest_string1(xs) = 
	case xs of
		[] => ""
	      | x::xs' => foldl (fn (f, s) => if (String.size s > String.size f) then f else s) x xs'

fun shortest_string2(xs) = 
	case xs of
		[] => ""
	      | x::xs' => foldl (fn (f, s) => if (String.size s >= String.size f) then f else s) x xs'

fun capital(character) =
	if ord(character) >= ord(#"a") andalso ord(character) <= ord(#"z") then chr(ord(character) - (ord(#"a") - ord(#"A")))
     	else character

fun capitals(str) = String.implode(map capital (String.explode str)) 

val shortest_capitalized = capitals o shortest_string2;

fun check_pat(p) = 
	let 
      		fun distinct(xs) =
			let 
				fun check(x : string) = List.exists(fn x1 => x = x1) 
				
			in 
	       			case xs of
       		 			[]     => true
       			 	      | x::xs' => if (check(x) xs') then false
       	  	            			  else distinct(xs')
			end
		fun strings_list(p) = 
       			case p of
				Variable x          => [x] 
		      	      | TupleP xs           => foldl (fn (a, b) => strings_list(a) @ b) [] xs
       	     		      | ConstructorP(s, pt) => strings_list(pt)
      	    		      | _                   => []		
  	
	in
    		distinct(strings_list(p))
  	end 

fun match(vl, pt) =
	case (vl, pt) of  
		( _ , Wildcard)                                 => SOME []
	      | ( _ , Variable x)                               => SOME [(x, vl)]
              | (Unit, UnitP)                                   => SOME []
	      | (Const vl1, ConstP pt1)                         => if (vl1 = pt1) then SOME [] else NONE
	      | (Tuple vls, TupleP pts)                         => let 
									fun all_ans f(lt) : (string * valu) list option =  
										let 
											val res = foldl(fn(x, xs)  => (case f(x) of 
																SOME y => y @ xs
															      | _      => []     ))									      		
											val non = List.exists(fn(a) => f(a) = NONE)							
    										in 
    											case lt of
      												[] => SOME []
      		      									      | _  => if non lt then NONE
          		      									      else SOME (res [] lt)
    										end 

								   in
									if List.length(vls) = List.length(pts) 
                              		     			   	then case all_ans match(ListPair.zip(vls, pts)) of
                                    										SOME vl2 => SOME vl2
                                   								      	      | _        => NONE
                              					   	else NONE
								   end    
	      | (Constructor(s1, val1), ConstructorP(s2, pat1)) => if (s1 = s2) then match(val1, pat1) else NONE
	      | ( _ , _ )                                       => NONE
