#!/bin/bash

threads=`cat /proc/cpuinfo | grep "processor" | wc -l`

source /etc/os-release

echo $ID $VERSION_ID -j $threads

rm -rf FileMd5Database

if [[ "$ID" == "ubuntu" && $VERSION_ID =~ (16|18) ]]; then
    sudo add-apt-repository ppa:ubuntu-toolchain-r/test
    sudo apt update
    sudo apt install g++-9 git cmake libboost-serialization-dev libboost-locale-dev libboost-filesystem-dev libboost-system-dev -y
    git clone https://github.com/iriszero48/FileMd5Database.git
    cd FileMd5Database && cmake FileMd5Database -DCMAKE_CXX_COMPILER=`which g++-9` && make -j $threads
elif [[ "$ID" == "ubuntu" && $VERSION_ID =~ (19|20) ]]; then
    sudo apt update
    sudo apt install g++ git cmake libboost-serialization-dev libboost-locale-dev libboost-filesystem-dev libboost-system-dev -y
    git clone https://github.com/iriszero48/FileMd5Database.git
    cd FileMd5Database && cmake FileMd5Database && make -j $threads
elif [[ "$ID" == "debian" || "$ID" == "raspbian" ]]; then
    sudo apt update
    sudo apt install g++ git cmake libboost-serialization-dev libboost-locale-dev libboost-filesystem-dev libboost-system-dev -y
    git clone https://github.com/iriszero48/FileMd5Database.git
    cd FileMd5Database && cmake FileMd5Database && make -j $threads
elif [[ "$ID" == "kali" ]]; then
    sudo apt update
    sudo apt install g++ git cmake libboost-serialization-dev libboost-locale-dev libboost-filesystem-dev libboost-system-dev -y
    git clone https://github.com/iriszero48/FileMd5Database.git
    cd FileMd5Database && cmake FileMd5Database && make -j $threads
elif [[ "$ID" == "gentoo" ]]; then
    sudo sh -c "echo 'dev-libs/boost static-libs' >> /etc/portage/package.use/boost"
    sudo emerge --ask --verbose dev-vcs/git dev-util/cmake dev-libs/boost
    git clone https://github.com/iriszero48/FileMd5Database.git
    cd FileMd5Database && cmake FileMd5Database && make -j $threads
else
    echo unsupport $ID
fi

if [[ -f fmd ]]; then
    echo "done."
else
    echo "failed."
fi
