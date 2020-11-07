#! /usr/bin/awk -f

BEGIN {
  purl = "/inet/tcp/0/202.38.93.111/0"
  ORS = RS = "\r\n\r\n"
  print "GET / HTTP/1.0" |& purl
  purl |& getline
  print $0
  while ( (purl |& getline ) > 0 ) {
    printf $0
  }
  close(purl)
}
