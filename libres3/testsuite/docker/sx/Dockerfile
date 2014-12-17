FROM debian:jessie
# build-dependencies for SX
ADD GPG-KEY-skylable.asc /root/
RUN echo 'deb http://cdn.skylable.com/debian/ jessie main' >/etc/apt/sources.list.d/skylable.list &&\
    apt-key add /root/GPG-KEY-skylable.asc &&\
    apt-get update &&\
    apt-get install -y sx openssl ca-certificates &&\
    apt-get clean

EXPOSE 443
