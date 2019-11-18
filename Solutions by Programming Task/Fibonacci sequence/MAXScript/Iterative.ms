fn fib x =
(
	if x < 1 then
	(
		0
	)
	else
	(
		prev = 0
		next = 1
		for i in 1 to x - 1 do
		(
			sum = prev + next
			prev = next
			next = sum
		)
		next
	)
)

messageBox(fib(10) as string)
