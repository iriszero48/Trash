echo "deb http://mirrordirector.raspbian.org/raspbian/ jessie main contrib non-free rpi" >> /etc/apt/sources.list
apt-get update
apt-get upgrade
apt-get autoremove

sudo apt-get install nginx php5-fpm php5-cli php5-curl php5-gd php5-mcrypt php5-cgi
sudo service nginx start
sudo service php5-fpm restart

cd /var/www/html
sudo git clone https://github.com/spoonysonny/pi-dashboard.git
sudo chown -R www-data pi-dashboard
