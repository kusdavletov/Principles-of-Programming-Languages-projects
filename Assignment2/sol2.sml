datatype expr = NUM of int
              | PLUS of expr * expr
              | MINUS of expr * expr

datatype formula = TRUE
                 | FALSE
                 | NOT of formula
                 | ANDALSO of formula * formula
                 | ORELSE of formula * formula
                 | IMPLY of formula * formula
                 | LESS of expr * expr

(* eval_expr: expr -> int *)

fun eval_expr(e) =
    case e of
          NUM i         => i
        | PLUS(e1, e2)  => (eval_expr e1) + (eval_expr e2)
        | MINUS(e1, e2) => (eval_expr e1) - (eval_expr e2)

(* eval: formula -> bool *)

fun eval(e) =
    case e of
          TRUE               => true
        | FALSE              => false
        | NOT(e1)            => not (eval e1)
        | ANDALSO(e1, e2)    => (eval e1) andalso (eval e2)
        | ORELSE(e1, e2)     => (eval e1) orelse (eval e2)
        | IMPLY(e1, e2)      => (not (eval e1)) orelse (eval e2)
        | LESS(expr1, expr2) => (eval_expr expr1) < (eval_expr expr2)


type name = string

datatype metro = STATION of name
               | AREA of name * metro
               | CONNECT of metro * metro

(* checkMetro: metro -> bool *)

fun checkMetro(e: metro) = 
	case e of
		STATION n1      => false
	      | CONNECT(m1, m2) => checkMetro(m1) andalso checkMetro(m2)
	      | AREA(n1, m1)    => case m1 of
			  		    STATION(n11)          => n1 = n11
					  | CONNECT(m11, m12)     => checkMetro(CONNECT(m11, m12)) orelse (checkMetro(AREA(n1, m11)) andalso checkMetro(AREA(n1, m12))) orelse (checkMetro(m11) andalso checkMetro(AREA(n1, m12))) orelse (checkMetro(AREA(n1, m11)) andalso checkMetro(m12))
					  | AREA(n11, m11)        => case m11 of 
									STATION n11_1         => n11_1 = n11 orelse n11_1 = n1
								      | CONNECT(m11_1, m12_1) => checkMetro(m1) orelse checkMetro(AREA(n1, m11)) orelse (checkMetro(AREA(n1, m11_1)) andalso checkMetro(AREA(n11, m12_1))) orelse (checkMetro(AREA(n1, m12_1)) andalso checkMetro(AREA(n11, m11_1)))
								      | AREA(n11_1, m11_1)    => checkMetro(m11) orelse checkMetro(AREA(n11, m11_1)) orelse checkMetro(AREA(n1, m11_1))


datatype 'a lazyList = nullList
                     | cons of 'a * (unit -> 'a lazyList)

fun seq(first, last) =  if last < first 
				then nullList
			else cons(first, fn() => seq(first+1, last))

fun infSeq(first) = cons(first, fn () => infSeq(first + 1))

fun firstN(lazyListVal, n) =
    	if (n = 0) then []
    	else
        	case lazyListVal of
          	      nullList      => []
          	    | cons(hd, tl)  => hd :: firstN(tl(), n-1)

fun Nth(lazyListVal, n) = 
	case lazyListVal of 
		nullList     => NONE 
	      | cons(hd, tl) => let fun f(head, tail, position) = 
  			              if position = 0 then NONE 
				      else if position = 1 then SOME(head) 
  				      else Nth(tl(), position - 1)
			        in 
			       	      f(hd, tl, n) 
			        end

fun filterMultiples(lazyListVal, n) =
	case lazyListVal of 
		nullList     => nullList
	      | cons(hd, tl) => let fun f(head, tail, number) = 
				      if (head mod number) = 0 then filterMultiples(tail(), number)
				      else cons(head, fn() => filterMultiples(tail(), number))
				in 
				      f(hd, tl, n) 
				end

fun filterNew pr nullList       = nullList
  | filterNew pr (cons(hd, tl)) = if pr hd then cons(hd, fn() => filterNew pr (tl()))
				  else filterNew pr (tl())

fun noDiv(prime) = filterNew(fn n => n mod prime <> 0)

fun sieve(lazyListVal) = 
	case lazyListVal of
		nullList     => nullList
	      | cons(hd, tl) => cons(hd, fn() => sieve(noDiv hd (tl())))

fun primes() = sieve(infSeq(2))