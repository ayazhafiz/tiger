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
