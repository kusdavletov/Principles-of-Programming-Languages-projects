fun map(f : int -> int, xs : int list) =
	if null xs then []
	else f(hd(xs))::map(f, tl xs)
