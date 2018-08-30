fun better_max(xs : int list) =
	if null xs then NONE
	else let val ans = better_max(tl xs)
		 in 
			if isSome(ans) andalso hd(xs) > valOf(ans) then SOME(hd(xs))
			else if isSome(ans) then ans
			else SOME(hd(xs))
		 end
