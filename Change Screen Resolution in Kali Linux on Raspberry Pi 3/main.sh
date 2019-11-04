mkdir /fat32
mount /dev/mmcblk0p1 /fat32/
echo "width:"
read w
echo "height:"
read h
echo "disable_overscan=1" >> /fat32/config.txt
echo "framebuffer_width=$w" >> /fat32/config.txt
echo "framebuffer_height=$h" >> /fat32/config.txt
echo "framebuffer_depth=32" >> /fat32/config.txt
echo "framebuffer_ignore_alpha=1" >> /fat32/config.txt
echo "hdmi_drive=2" >> /fat32/config.txt
umount /fat32
echo "any key to reboot"
read n
reboot
