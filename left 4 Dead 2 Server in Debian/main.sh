apt-get install lib32gcc1
mkdir ~/steamcmd
cd ~/steamcmd
wget https://steamcdn-a.akamaihd.net/client/installer/steamcmd_linux.tar.gz
tar -zxvf steamcmd_linux.tar.gz

./steamcmd.sh

#login anonymous
#force_install_dir /root/l4d2
#app_update 222860 validate

#exit

nano /root/l4d2/left4dead2/cfg/server.cfg

#hostname "服务器的名字"
#sv_steamgroup "steam组的id"

/root/l4d2/srcds_run -game left4dead2 +exec server.cfg
