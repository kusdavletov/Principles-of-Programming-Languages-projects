fun merge(first : int list, second : int list) =
    if null first orelse null second
    then if null first andalso null second then []
         else if null first then second
         else first
    else if hd(first) > hd(second) then hd(second) :: merge(first, tl(second))
    else hd(first) :: merge(tl(first), second)

fun reverse(original : int list) =
    if null original then []
    else reverse(tl(original)) @ [hd(original)]

fun sigma(start : int, finish : int, func : (int -> int)) =
    if start > finish then 0
    else func(start) + sigma(start + 1, finish, func)

fun digits(original : int) =
    if original < 10 then [original]
    else digits(original div 10) @ [original mod 10]

fun sum(arr : int list) =
    if null arr then 0
    else hd(arr) + sum(tl(arr))

fun additivePersistence(original : int) =
    if original < 10 then 0
    else 1 + additivePersistence(sum(digits(original)))

fun digitalRoot(original : int) =
    if original < 10 then original
    else digitalRoot(sum(digits(original)))
