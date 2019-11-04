cd /opt
apt-get install git
git clone git://github.com/sqlmapproject/sqlmap.git
echo "alias sqlmap='python /opt/sqlmap/sqlmap.py'" >> ~/.bashrc
