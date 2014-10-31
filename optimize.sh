#!/bin/bash
cd Optimisanaitanu
fileIn=$1 #unoptimised code

fileOut=$2 #optimised code

opt=$3 #what optimiser(s) to run
case $opt in
    1) echo "Doing nothing" ;; #do nothing
	2) echo printUnreachableFile \"../$fileIn\" | ghci Unreachable.hs -v0 > ../$fileOut ;; #do unreachable
	3) echo deadcode_file \"../$fileIn\" | ghci Deadcode.hs -v0 > ../$fileOut ;; #do deadcode
	4) echo printRedundantFile \"../$fileIn\" | ghci Redundant.hs -v0 > ../$fileOut ;;#do redundant
	5) echo printUnreachableFile \"../$fileIn\" | ghci Unreachable.hs -v0 > ../temp.intermediate
	echo deadcode_file \"../temp.intermediate\" | ghci Deadcode.hs -v0 > ../$fileOut
	rm ../temp.intermediate ;;
	6) echo printUnreachableFile \"../$fileIn\" | ghci Unreachable.hs -v0 > ../temp.intermediate
	echo printRedundantFile \"../temp.intermediate\" | ghci Redundant.hs -v0 > ../$fileOut
	rm ../temp.intermediate ;;
	7) echo deadcode_file \"../$fileIn\" | ghci Deadcode.hs -v0 > ../temp.intermediate
	echo printRedundantFile \"../temp.intermediate\" | ghci Redundant.hs -v0 > ../$fileOut
	rm ../temp.intermediate ;;
	8) echo printUnreachableFile \"../$fileIn\" | ghci Unreachable.hs -v0 > ../temp.intermediate
	echo printRedundantFile \"../temp.intermediate\" | ghci Redundant.hs -v0 > ../temp.intermediate2
	echo deadcode_file \"../temp.intermediate2\" | ghci Deadcode.hs -v0 > ../$fileOut
	rm ../temp.intermediate
	rm ../temp.intermediate2 ;;
	*) echo "Invalid Option" ;;
esac

#1 - nothing
#2 - unreachable
#3 - deadcode
#4 - redundant
#5 - unreachable deadcode
#6 - unreachable redundant
#7 - deadcode redundant
#8 - unreachable deadcode redundant
cd ../