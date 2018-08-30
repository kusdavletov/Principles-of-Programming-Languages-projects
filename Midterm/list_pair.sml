fun sum_list(xs : (int * int) list) = 
	if null xs then (0,0)
	else 
		let val (fi, se) = sum_list(tl xs)
		in 
			((#1 (hd(xs))) + fi, (#2 (hd(xs))) + se)
		end

fun firsts(xs : (int * int) list) = 
	if null xs then []
	else (#1 (hd(xs))) :: firsts(tl xs)

fun seconds(xs : (int * int) list) = 
	if null xs then []
	else (#2 (hd(xs))) :: firsts(tl xs)


