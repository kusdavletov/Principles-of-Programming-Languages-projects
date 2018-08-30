fun pow(a : int, n : int) = 
	if n = 0 then 1
	else a * pow(a, n - 1)

fun cube(a : int) = 
	pow(a, 3)

val power_of_three = cube(5);
