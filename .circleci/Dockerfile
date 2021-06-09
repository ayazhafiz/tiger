FROM ocaml/opam
SHELL ["/bin/bash", "-c"]

RUN opam update
RUN opam switch 4.12
RUN opam install -y menhir
RUN opam install -y dune
