FROM ocaml/opam
SHELL ["/bin/bash", "-c"]

RUN opam update
RUN opam switch 4.12
RUN opam install -y menhir
RUN opam install -y dune
RUN opam install -y alcotest

RUN sudo apt-get update
RUN sudo apt-get install -y apt-utils
RUN sudo apt-get install -y nasm

RUN sudo apt-get install -y python2.7 python-pip
RUN sudo apt-get install -y wget
RUN sudo apt-get install -y software-properties-common
RUN sudo add-apt-repository 'deb http://apt.llvm.org/buster/ llvm-toolchain-buster-11 main'
RUN wget -O - https://apt.llvm.org/llvm-snapshot.gpg.key|sudo apt-key add -
RUN sudo apt-get update
RUN sudo apt-get install -y libllvm-11-ocaml-dev libllvm11 llvm-11 llvm-11-dev llvm-11-doc llvm-11-examples llvm-11-runtime
RUN sudo apt-get install -y cmake
RUN opam install -y llvm
