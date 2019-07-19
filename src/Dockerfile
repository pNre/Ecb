FROM amazonlinux:latest
RUN yum update -y
RUN yum install patch tar unzip make m4 gzip git gcc bzip2 pkgconfig openssl-devel which gmp gmp-devel libffi-devel -y
WORKDIR /tmp
RUN curl https://raw.githubusercontent.com/ocaml/opam/master/shell/install.sh -o install.sh
RUN chmod +x install.sh
RUN echo | ./install.sh
RUN groupadd -r opam -g 1000 && useradd -u 1000 -r -g opam -m -d /app -s /sbin/nologin -c "opam user" opam
RUN chmod 755 /app
USER opam
WORKDIR /app
RUN opam init --disable-sandboxing --compiler=4.08.0 -y
RUN opam update
RUN opam upgrade -y
RUN opam install -y dune depext
RUN eval `opam config env`
RUN opam depext async async_ssl cohttp-async ppx_deriving ppx_deriving_yojson yojson ssl lambdasoup
RUN opam pin -y ssl 0.5.7
RUN opam pin -y nocrypto "https://github.com/mirleft/ocaml-nocrypto.git#dune"
RUN opam install -y async async_ssl cohttp-async ppx_deriving ppx_deriving_yojson yojson lambdasoup
COPY --chown=opam:opam . /app
RUN opam config exec -- dune clean
RUN opam config exec -- dune build @all --profile=static
RUN gcc -shared -fPIC -ldl -o hook.so hook.c
