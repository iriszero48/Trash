BEGIN {
    prefix = ""
}

{
	if (NF != 2) {
		print "NF: " NF ", " $0
	}

	if ($1 !~ /[0-9a-z_.]/) {
		print "v1: " $0
		exit
	}

	if ($2 !~ /[a-zA-Z0-9]+/) {
		print "v2: " $0
		exit
	}

    printf("{\"%s_\":\"%s\",\"%s_\": \"%s\"}\n", prefix, $1, prefix, $2)
}
