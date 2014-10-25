#!/bin/bash
cd interpreter
file=$1
shift
line=[$*]
line=`echo $line | tr ' ' ,`
echo printInterpreted \"../$file\" $line | ghci Interpreter.hs -v0 
cd ../
