echo "Enter username: "
read username
echo "$username ALL=(ALL:ALL) ALL" >> /etc/sudoers
