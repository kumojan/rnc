#!/bin/bash
assert() {
  expected="$1"
  input="$2"

  ./target/debug/rnc "$input" > bind/tmp.s
  docker run --rm --mount type=bind,src=$PWD/bind,dst=/home/user --workdir /home/user compilerbook /bin/bash -c "cc -o tmp tmp.s; ./tmp"
  actual="$?"

  if [ "$actual" = "$expected" ]; then
    echo "$input => $expected"
  else
    echo "$input => $expected expected, but got $actual"
    exit 1
  fi
}

assert 0 0
assert 42 42
assert 21 "5+20-4"
assert 10 "1+ 2 + 3 +   4  "

echo OK