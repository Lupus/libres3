FROM libres3_test__libres3

# need an SX for sxadm to join new nodes
ADD GPG-KEY-skylable.asc /root/
RUN echo 'deb http://cdn.skylable.com/debian/ jessie main' >/etc/apt/sources.list.d/skylable.list &&\
    apt-key add /root/GPG-KEY-skylable.asc &&\
    apt-get update &&\
    apt-get install -y openssl sx ca-certificates &&\
    apt-get clean

ADD join.sh /root/
CMD ["/root/join.sh"]
