#!/bin/bash

case $1 in
  "unittest")
    dune test
    ;;
  "uitest")
    shift
    dune exec -- test/compiler.exe "$@"
    ;;
  "diff")
    git diff --diff-filter=AM --no-index test/baseline/golden test/baseline/local
    ;;
  "lkg")
    gcc -c -o "runtime/lkg/$(gcc -dumpmachine).o" runtime/runtime.c
    ;;
  "lkg-docker")
    cid="$(docker run -d -ti ayazhafiz/tigercompiler /bin/bash)"
    h="/home/opam"
    docker cp runtime/runtime.c "$cid:$h/runtime.c"
    docker exec -t "$cid" bash -c "mkdir $h/extract"
    docker exec -t "$cid" bash -c "gcc -c -o $h/extract/\$(gcc -dumpmachine).o $h/runtime.c"
    docker cp "$cid:$h/extract" /tmp/extract
    ls /tmp/extract
    mv /tmp/extract/*.o runtime/lkg
    rm -rf /tmp/extract
    docker stop "$cid" >/dev/null
    docker rm "$cid" >/dev/null
    ;;
  *)
    echo "Unknown command \"$1\""
    exit 1
    ;;
esac
