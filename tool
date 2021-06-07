#!/bin/sh

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
  *)
    echo "Unknown command \"$1\""
    exit 1
    ;;
esac
