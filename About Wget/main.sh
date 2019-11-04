#TYPE IS HTTPS WE HAVE USERNAME PASS AND SPEED
wget -c -e --limit-rate=$SPEED robots=off -x --user=$USERNAME --password=$PASSWORD -m --no-check-certificate $REMOTESERVER

#TYPE IS HTTPS WE HAVE USERNAME AND PASS BUT NOT SPEED
wget -c -e robots=off -x --user=$USERNAME --password=$PASSWORD -m --no-check-certificate $REMOTESERVER

#TYPE IS HTTPS WE DO NOT HAVE USERNAME PASS BUT WE DO HAVE SPEED
wget -c -e --limit-rate=$SPEED robots=off -x -m --no-check-certificate $REMOTESERVER

#TYPE IS HTTPS WE NO USERNAME PASSWORD OR SPEED
wget -c -e robots=off -x -m --no-check-certificate $REMOTESERVER

#TYPE IS HTTP WE HAVE USERNAME PASS AND SPEED
wget -c -e --limit-rate=$SPEED robots=off -x --user=$USERNAME --password=$PASSWORD -m $REMOTESERVER

#TYPE IS HTTP WE HAVE USERNAME AND PASS BUT NOT SPEED
wget -c -e robots=off -x --user=$USERNAME --password=$PASSWORD -m $REMOTESERVER

#TYPE IS HTTP WE DO NOT HAVE USERNAME PASS BUT WE DO HAVE SPEED
wget -c -e --limit-rate=$SPEED robots=off -x -m $REMOTESERVER

#TYPE IS HTTP WE NO USERNAME PASSWORD OR SPEED
wget -c -e robots=off -x -m $REMOTESERVER
