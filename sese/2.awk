{
	if (NR == 1) next

	if (NF != 2) {
		print "NF: " NF ", " $0
	}

	p = $1
	u = $2

	if ($1 !~ /[0-9]+/) {
		# print "v1: " $0
		# next
		p = -1
	}

	if ($2 !~ /[0-9]+/) {
		# print "v2: " $0
		# next
		u = -1
	}

	if (p == -1 && u == -1) {}
	else if (u == -1) {
		print "{\"\":\"" p "\",\"\":null}"
	} else {
		print "{\"\":\"" p "\",\"\":\"" u "\"}"
	}
}
