#!/bin/bash
cargo build
./target/debug/rnc tests/tests.c > bind/tmp.s
CMD="gcc -static -o tmp tmp.s; ./tmp"
docker run --rm --mount type=bind,src=$PWD/bind,dst=/home/user --workdir /home/user compilerbook /bin/bash -c "$CMD"