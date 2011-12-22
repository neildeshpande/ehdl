#!/bin/bash 

alltests="adder choose dumb_inv func_test3 func_test9 gcd \
if_test1 if_test3 invoke_function subbus switchcase_test1 two_bit_alu \
switchcase_test1 ops_test1 factorial four_one_mux priority_encoder \
trafficlight fibonacci uminus ripple_carry sieve while1 while2 while3 \
while4"


usage="\nregression.sh all \nOR \nregression.sh if_test1 if_test2 ...\n"

if [ $# -eq 0 ] 
then 
    echo -e $usage
    exit
fi

comppath="../src/ehdl "
testpath="./"
mainpath="../src"


if [ "$1" = "all" ]
then 
    tests=$alltests
else
    tests=$@
fi 

for var in $tests
do 
    
    $comppath $testpath$var.ehdl > temp.txt
    diff main.vhd ../golden/$var.vhd > temp.txt 
    difflines=`wc -l temp.txt | cut -f1 -d" "`


    if [ $difflines -eq 0 ] 
    then
	echo "PASSED regression test for $testpath$var.ehdl ..."  
    else 
	echo "FAILED regression test for $testpath$var.ehdl ..."  
	cat temp.txt 
    fi
    
done




