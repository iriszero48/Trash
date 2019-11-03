wget -q -O- https://github.com/hugsy/gef/raw/master/gef.sh | sh
wget -q -O ~/.gdbinit-gef.py https://github.com/hugsy/gef/raw/master/gef.py
echo source ~/.gdbinit-gef.py >> ~/.gdbinit
