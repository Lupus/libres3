FROM debian:jessie
# cachable part: RUN commands
RUN apt-get update && apt-get -y install\
        git build-essential\
        libyajl-dev libcurl4-openssl-dev libfcgi-dev libssl-dev\
        libwww-perl libjson-xs-perl libdigest-hmac-perl liburi-perl\
        ccache
RUN useradd -m build && mkdir /opt/sx && chown build:build /opt/sx
USER build
WORKDIR /home/build
RUN git clone http://git.skylable.com/sx
WORKDIR /home/build/sx
RUN ./configure --prefix=/opt/sx/1 CC="ccache gcc" && make -j10
RUN make -j10 check install
RUN git clean -x -f -d && ./configure --prefix=/opt/sx/2 CC="ccache gcc" && make -j10 install
RUN git clean -x -f -d && ./configure --prefix=/opt/sx/3 CC="ccache gcc" && make -j10 install
RUN git clean -x -f -d && ./configure --prefix=/opt/sx/4 CC="ccache gcc" && make -j10 install

# libres3 build
USER root
RUN apt-get install -y opam aspcud ocaml-native-compilers
# for testing
RUN apt-get install -y s3cmd/jessie-backports python-boto
RUN mkdir /opt/libres3 && chown build:build /opt/libres3

USER build
RUN opam init -y
ENV OPAMJOBS 9

RUN opam install -e debian libres3 >/home/build/packages
RUN echo libpcre3-dev camlp4-extra >>/home/build/packages
USER root
RUN apt-get install -y `cat /home/build/packages`

USER build
RUN opam install -y --deps-only libres3
WORKDIR /home/build
RUN git clone http://git.skylable.com/libres3
WORKDIR /home/build/libres3
RUN opam install ounit

# appears to be uncachable: ADD commands
ADD setup-sx.sh /home/build/
ADD build-libres3.sh /home/build/
ADD run-test.sh /home/build/
RUN /home/build/setup-sx.sh
RUN /home/build/build-libres3.sh master
ADD test-libres3.sh /home/build/

USER root
RUN apt-get install -y uuid-runtime
USER build

ENTRYPOINT ["/home/build/test-libres3.sh"]
