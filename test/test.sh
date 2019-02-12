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

# number
try 0 "0;"
try 36 "36;"
try 42 "42;"

# add, sub
try 21 "5+20-4;"
try 68 "5+20-4+59-12;"
try 30 "10-20-40+80;"

# mul, div
try 25 "5*5;"
try 125 "5*5*5;"
try 5 "65/13;"
try 77 "999999/999/13;"
try 0 "99/100;"
try 1 "100/100;"

# four arithmetic
try 106 "2+4*6+8*10;"
try 66 "2*4+6*8+10;"
try 28 "20/2+5*4-8/4;"

# parentheses
try 21 "(21);"
try 45 "(2+3)*(4+5);"
try 10 "(2*(4+6)*8+10)/17;"
try 74 "1+(2*3)-(4*5/10)+7+(8*9)-10;"
try 70 "(1+2*(2+6)+(8+2)+(5+(8-(1+((1+(1+1))+1)+(1+((1-(9/3)+(6+(1+(5+1))-1)/(1+1))+1))+1)+(5+2)*(2+2)))+5)+9;"

# assignment, statement
try 6 "a=3;a*2;"
try 15 "a=5;b=a+5;a+b;"
try 27 "a=b=c=e=d=9;f=a+b+c*2+d+e;f/2;"
try 25 "z=(y=5)*4;y+z;"

# ==, !=
try 1 "a=10;b=10;c=(a==b);"
try 0 "a=10;b=10;c=(a!=b);"
try 0 "a=10;b=20;c=(a==b);"
try 1 "a=10;b=20;c=(a!=b);"
try 3 "a=10;c=b=20;e=(a==b)+(b==c)+(c==a)+(a!=b)+(b!=c)+(c!=a);"

# <, >, <=, >=
try 0 "a=15;b=10;c=(a<b);"
try 0 "a=15;b=15;c=(a<b);"
try 1 "a=15;b=20;c=(a<b);"
try 1 "a=15;b=10;c=(a>b);"
try 0 "a=15;b=15;c=(a>b);"
try 0 "a=15;b=20;c=(a>b);"
try 0 "a=15;b=10;c=(a<=b);"
try 1 "a=15;b=15;c=(a<=b);"
try 1 "a=15;b=20;c=(a<=b);"
try 1 "a=15;b=10;c=(a>=b);"
try 1 "a=15;b=15;c=(a>=b);"
try 0 "a=15;b=20;c=(a>=b);"

echo "*** OK ***"
