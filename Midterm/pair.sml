fun swap(a : (int * int)) = 
	(#2 a, #1 a)

fun sort_pair(a: (int * int)) = 
	if (#1 a) < (#2 a) then a
	else (#2 a, #1 a)

fun sum_two_pairs(a : (int * int), b : (int * int)) = 
	(#1 a + #1 b, #2 a + #2 b)
