#!/bin/bash
if [ "$1" != "--no-build" ]; then
  cargo build
fi
./target/debug/rnc tests/tests.c > bind/tmp.s
CMD='echo "int char_fn() { return 257; } int static_fn() { return 5; } int ext3 = 5;" |  gcc -xc -c -o tmp2.o -;'
CMD+="gcc -static -o tmp tmp.s tmp2.o && ./tmp"

docker run --rm --mount type=bind,src=$PWD/bind,dst=/home/user --workdir /home/user compilerbook /bin/bash -c "$CMD"