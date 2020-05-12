#!/bin/bash

# <<EOFとやると、その次の行からEOFまでがcatに投入される。
CMD="cat <<EOF | gcc -xc -c -o tmp2.o -
int ret3() { return 3; }
int ret5() { return 5; }
EOF"
docker run --rm --mount type=bind,src=$PWD/bind,dst=/home/user --workdir /home/user compilerbook /bin/bash -c "$CMD"

assert() {
  expected="$1"
  input="$2"

  ./target/debug/rnc "$input" > bind/tmp.s
  CMD="gcc -static -o tmp tmp.s tmp2.o; ./tmp"
  docker run --rm --mount type=bind,src=$PWD/bind,dst=/home/user --workdir /home/user compilerbook /bin/bash -c "$CMD"
  actual="$?"

  if [ "$actual" = "$expected" ]; then
    echo "$input => $expected"
  else
    echo "$input => $expected expected, but got $actual"
    exit 1
  fi
}

cargo build
assert 0 'return 0;'
assert 42 'return 42;'
assert 21 'return 5+20-4;'
assert 41 'return  12 + 34 - 5 ;'
assert 47 'return 5+6*7;'
assert 15 'return 5*(9-6);'
assert 4 'return (3+5)/2;'
assert 10 'return -10+20;'
assert 10 'return - -10;'
assert 10 'return - - +10;'

assert 0 'return 0==1;'
assert 1 'return 42==42;'
assert 1 'return 0!=1;'
assert 0 'return 42!=42;'

assert 1 'return 0<1;'
assert 0 'return 1<1;'
assert 0 'return 2<1;'
assert 1 'return 0<=1;'
assert 1 'return 1<=1;'
assert 0 'return 2<=1;'

assert 1 'return 1>0;'
assert 0 'return 1>1;'
assert 0 'return 1>2;'
assert 1 'return 1>=0;'
assert 1 'return 1>=1;'
assert 0 'return 1>=2;'

assert 3 'a=3; return a;'
assert 8 'a=3; z=5; return a+z;'

assert 1 'return 1; 2; 3;'
assert 2 '1; return 2; 3;'
assert 3 '1; 2; return 3;'

assert 3 'foo=3; return foo;'
assert 8 'foo123=3; bar=5; return foo123+bar;'
assert 42 "a=42; if (a==42) return a; else a = a+1; return a;"
assert 42 "a=41; if (a==42) return a; else a = a+1; return a;"

assert 55 "a=0;b=0;while(b<10) a = a+(b = b+1); return a;"
assert 45 "a = 0;for(i=0; i<10; i = i+1) a = a + i; return a;"
assert 55 "a = 0;i=0;for(;i<10;) a = a + (i=i+1); return a;"
assert 42 "for(;;) return 42;"
assert 89 "a=1;b=1;for(i=0;i<10;i=i+1) {c=b;b=a+b;a=c;} return a;"

assert 3 'return ret3();'
assert 5 'return ret5();'
echo OK