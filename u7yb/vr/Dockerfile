FROM ubuntu:20.04

WORKDIR /code
ENV TZ=Asia/Shanghai \
    DEBIAN_FRONTEND=noninteractive
RUN echo "deb http://mirrors.aliyun.com/ubuntu/ focal main restricted universe multiverse\ndeb-src http://mirrors.aliyun.com/ubuntu/ focal main restricted universe multiverse\ndeb http://mirrors.aliyun.com/ubuntu/ focal-security main restricted universe multiverse\ndeb-src http://mirrors.aliyun.com/ubuntu/ focal-security main restricted universe multiverse\ndeb http://mirrors.aliyun.com/ubuntu/ focal-updates main restricted universe multiverse\ndeb-src http://mirrors.aliyun.com/ubuntu/ focal-updates main restricted universe multiverse\ndeb http://mirrors.aliyun.com/ubuntu/ focal-proposed main restricted universe multiverse\ndeb-src http://mirrors.aliyun.com/ubuntu/ focal-proposed main restricted universe multiverse\ndeb http://mirrors.aliyun.com/ubuntu/ focal-backports main restricted universe multiverse\ndeb-src http://mirrors.aliyun.com/ubuntu/ focal-backports main restricted universe multiverse" > /etc/apt/sources.list && \
    apt-get clean && apt-get update && \
    apt-get update && \
    apt-get -y install libavcodec-dev libavfilter-dev libavformat-dev libavresample-dev libavutil-dev wget g++-10 pkg-config
RUN mkdir nlohmann && \
    wget https://hub.fastgit.org/nlohmann/json/releases/download/v3.10.4/json.hpp -O nlohmann/json.hpp && \
    cp -r nlohmann /usr/include/x86_64-linux-gnu/
RUN g++-10 *.cpp -o vr -std=c++17 -lpthread \
    `pkg-config --cflags --libs libavformat libswscale libavfilter libavcodec libswresample libswscale libavutil`
CMD bash -i
