echo "mode:"
echo "hdmi_mode=46   1440x900        reduced blanking"
echo "hdmi_mode=65   1856x1392 60Hz"
echo "hdmi_mode=82   1080p     60Hz"
echo "select mode:"
read m
echo "hdmi_group=2" >> /boot/config.txt
echo "hdmi_mode=$m" >> /boot/config.txt
echo "hdmi_ignore_edid=0xa5000080" >> /boot/config.txt
echo "Any key to reboot"
read x
reboot
