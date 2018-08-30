datatype human = Name of string
				 | Surname of string
				 | Age of int
				 | Birthday of (int*int*int)

fun showData(a : human) = 
	case a of Name s => String.size s
			  | Surname s => String.size s
			  | Age s => let fun sum_digits(s) = 
			  				 if s <= 0 then 0
							 else (s mod 10) + sum_digits(s div 10)
			  			 in
						 	sum_digits(s)
						 end
			  | Birthday s => let val (x,y,z) = s
			  				 in
							 	x + y + z
							 end
			  					 
datatype suit = Club | Diamond | Heart | Spade
datatype rank = Jack | Queen | King | Ace | Num of int

val ss = (Club, Queen)
