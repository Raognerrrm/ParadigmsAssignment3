#!/bin/bash
cd Optimisanaitanu
fileIn=$1
fileOut=$2
opt=$3
case $opt in
    1) echo "Doing nothing" ;;
	2) echo printUnreachableFile \"../$fileIn\" | ghci Unreachable.hs -v0 > ../$fileOut ;;
	3) echo deadcode_file \"../$fileIn\" | ghci Deadcode.hs -v0 > ../$fileOut ;;
	4) echo printRedundantFile \"../$fileIn\" | ghci Redundant.hs -v0 > ../$fileOut ;;
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
cd ../