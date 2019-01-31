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

try 0 0
try 36 36
try 42 42
try 21 "5+20-4"
try 68 "5+20-4+59-12"
try 30 "10-20-40+80"

echo "*** OK ***"
