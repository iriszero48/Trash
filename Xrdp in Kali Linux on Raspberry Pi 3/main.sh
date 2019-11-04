apt-get install xrdp
apt-get install xfce4
echo "echo \"xfce4-session\" >~/.xsession" >> /etc/xrdp/startwm.sh
service xrdp restart
update-rc.d xrdp enable
