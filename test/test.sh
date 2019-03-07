#!/bin/bash

cd $(dirname $0)

try() {
  expected="$1"
  input="$2"

  stack exec hcc-exe "$input" > tmp.s
  gcc -o tmp tmp.s
  ./tmp
  actual="$?"

  if [ "$actual" = "$expected" ]; then
    echo "$input => $actual"
  else
    echo "$expected expecd, but got $actual"
    exit 1
  fi
}

tryFuncCall() {
  expected="$1"
  input="$2"
  funcdef="$3"

  stack exec hcc-exe "$input" > tmp.s
  echo "$funcdef" > foo.c
  gcc -o a.out tmp.s foo.c
  ./a.out
  actual="$?"

  if [ "$actual" = "$expected" ]; then
    echo "$input => $actual"
  else
    echo "$expected expecd, but got $actual"
    exit 1
  fi
}

# number
try 0 "0;"
try 36 "36;"
try 42 "42;"

# add, sub
try 21 "5 + 20 - 4;"
try 68 "5 + 20 - 4 + 59 - 12;"
try 30 "10 - 20 - 40 + 80;"

# mul, div
try 25 "5 * 5;"
try 125 "5 * 5 * 5;"
try 5 "65 / 13;"
try 77 "999999 / 999 / 13;"
try 0 "99 / 100;"
try 1 "100 / 100;"

# four arithmetic
try 106 "2 + 4*6 + 8*10;"
try 66 "2*4 + 6*8 + 10;"
try 28 "20/2 + 5*4 - 8/4;"

# parentheses
try 21 "(21);"
try 45 "(2+3)*(4+5);"
try 10 "(2*(4+6)*8+10)/17;"
try 74 "1 + (2*3) - (4*5/10) + 7 + (8*9) - 10;"
try 70 "(1+2*(2+6)+(8+2)+(5+(8-(1+((1+(1+1))+1)+(1+((1-(9/3)+(6+(1+(5+1))-1)/(1+1))+1))+1)+(5+2)*(2+2)))+5)+9;"

# assignment, statement
try 6 "a=3; a*2;"
try 15 "a=5; b=a+5; a+b;"
try 27 "a=b=c=e=d=9; f=a+b+c*2+d+e; f/2;"
try 25 "zz=(yy=5)*4; yy + zz;"
try 1 "foo=1; bar=3; hoge=6; huga=5; foo= 2 * huga; bar = (foo * huga) - hoge * bar * 2; bar == 14;"

# ==, !=
try 1 "a=10; b=10; c=(a==b);"
try 0 "a=10; b=10; c=(a!=b);"
try 0 "a=10; b=20; c=(a==b);"
try 1 "a=10; b=20; c=(a!=b);"
try 3 "a=10; c=b=20; e=(a==b)+(b==c)+(c==a)+(a!=b)+(b!=c)+(c!=a);"

# <, >, <=, >=
try 0 "a=15; b=10; c=(a<b);"
try 0 "a=15; b=15; c=(a<b);"
try 1 "a=15; b=20; c=(a<b);"
try 1 "a=15; b=10; c=(a>b);"
try 0 "a=15; b=15; c=(a>b);"
try 0 "a=15; b=20; c=(a>b);"
try 0 "a=15; b=10; c=(a<=b);"
try 1 "a=15; b=15; c=(a<=b);"
try 1 "a=15; b=20; c=(a<=b);"
try 1 "a=15; b=10; c=(a>=b);"
try 1 "a=15; b=15; c=(a>=b);"
try 0 "a=15; b=20; c=(a>=b);"

#function
tryFuncCall 1 "a = foo(); a==42;" "int foo(){return 42;}"
tryFuncCall 1 "a = foo(1); a==1;" "int foo(int a){return a==1;}"
tryFuncCall 1 "a = foo(1, 2); a==2;" "int foo(int a, int b){return (a==1)+(b==2);}"
tryFuncCall 1 "a = foo(1, 2, 3); a==3;" "int foo(int a, int b, int c){return (a==1)+(b==2)+(c==3);}"
tryFuncCall 1 "a = foo(1, 2, 3, 4); a==4;" "int foo(int a, int b, int c, int d){return (a==1)+(b==2)+(c==3)+(d==4);}"
tryFuncCall 1 "a = foo(1, 2, 3, 4, 5); a==5;" "int foo(int a, int b, int c, int d, int e){return (a==1)+(b==2)+(c==3)+(d==4)+(e==5);}"
tryFuncCall 1 "a = foo(1, 2, 3, 4, 5, 6); a==6;" "int foo(int a, int b, int c, int d, int e, int f){return (a==1)+(b==2)+(c==3)+(d==4)+(e==5)+(f==6);}"
tryFuncCall 1 "a = foo(1, 2, 3, 4, 5, 6, 7); a==7;" "int foo(int a, int b, int c, int d, int e, int f, int g){return (a==1)+(b==2)+(c==3)+(d==4)+(e==5)+(f==6)+(g==7);}"
tryFuncCall 1 "a = foo(1, 2, 3, 4, 5, 6, 7, 8); a==8;" "int foo(int a, int b, int c, int d, int e, int f, int g, int h){return (a==1)+(b==2)+(c==3)+(d==4)+(e==5)+(f==6)+(g==7)+(h==8);}"

echo "*** OK ***"
