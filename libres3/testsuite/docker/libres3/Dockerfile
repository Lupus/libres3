FROM debian:jessie
# build-dependencies for LibreS3
# opam: not strictly required, but allows building dependencies separately
# aspcud: ensures opam provides an optimal install solution
# ocaml-native-compilers : ensure native ocamlopt is used (faster builds)
RUN apt-get update && apt-get install --no-install-recommends -y\
        git gcc make libc-dev\
        opam aspcud ocaml-native-compilers && apt-get clean
ENV OPAMJOBS 9
ENV HOME /root
RUN opam init -y && apt-get update && apt-get install --no-install-recommends -y\
        `opam install -e debian libres3`\
        libpcre3-dev camlp4-extra &&\
        apt-get clean

RUN opam install -y --deps-only libres3 &&\
    opam install ounit bisect

RUN apt-get update && apt-get install -y --no-install-recommends s3cmd python-boto uuid-runtime && apt-get clean

ADD libres3.tar /usr/src/libres3/

WORKDIR /usr/src/libres3/libres3
RUN eval `opam config env` &&\
        ./configure --disable-docs --enable-tests --override ocamlbuildflags '-j 0'\
        && make all install
EXPOSE 8443
