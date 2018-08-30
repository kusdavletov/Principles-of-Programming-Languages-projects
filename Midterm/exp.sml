datatype exp = Constant of int
			   | Negate of exp
			   | Add of exp * exp
			   | Multiply of exp * exp
fun eval(x : exp) = 
	case x of Constant(e) => e 
			  | Negate(e) => ~(eval(e))
			  | Add(e1, e2) => eval(e1) + eval(e2)
			  | Multiply(e1, e2) => eval(e1) * eval(e2)
