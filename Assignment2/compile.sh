#!/bin/bash
cd compiler
echo printCompiled \"../$1\" | ghci Compiler.hs -v0 > ../$2
cd ../