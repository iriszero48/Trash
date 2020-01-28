FROM ubuntu
MAINTAINER iriszero48 version: 0.1
RUN sed -i "s/archive.ubuntu.com/mirrors.aliyun.com/g" /etc/apt/sources.list && \
 sed -i "s/security.ubuntu.com/mirrors.aliyun.com/g" /etc/apt/sources.list
RUN apt-get update && apt-get -y install nginx libnginx-mod-rtmp
EXPOSE 1935
RUN echo "rtmp{server{listen 1935;chunk_size 4096;application live {live on;}}}" >> /etc/nginx/nginx.conf
CMD service nginx start && bash
