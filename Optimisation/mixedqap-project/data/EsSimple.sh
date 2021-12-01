#!/bin/bash

exeDIR=../code/cpp/build
instanceDIR=../instances

Nmin=20
Nmax=100
instanceType="uni"
instanceSeed=1
listBounds="0 1"

nRun=30
timeLimit=5

EXE=EsSimple

output_performance=performance_${EXE}.csv
output_solution=solution_${EXE}.dat

> ${output_solution}
echo instanceType boundsType A B instanceSeed n idRun fitness > ${output_performance}
for boundsT in ${listBounds}
do
	if [ ${boundsT} -eq 0 ] ;
	then
		A=1
		B=100
	else
		A=-100
		B=100
	fi

	min=0
	max=0
	sum=0

	for((n=${Nmin};n<=${Nmax};n++))
	do
		instanceName=${instanceDIR}/mixedQAP_${instanceType}_${n}_${A}_${B}_${instanceSeed}.dat

		for((i=1;i<=${nRun};i++))
		do
			echo ${instanceName} ${i}
			${exeDIR}/${EXE} ${instanceName} ${i} ${timeLimit} > tmp

			cat tmp >> ${output_solution}

			tmp=`cat tmp`
			sum=$(echo "$sum + $tmp" |bc)

			if [ $(echo "$min == 0" | bc) -ne 0 ] ;
			then
				min=${tmp}
				max=${tmp}
			fi

			if [ $(echo "$min > $tmp" | bc) -ne 0 ] ;
			then
				min=${tmp}
			fi

			if [ $(echo "$max < $tmp" | bc) -ne 0 ] ;
			then
				max=${tmp}
			fi

			echo -n ${instanceType}_${boundsT}_${A}_${B}_${n} ${i}' ' >> ${output_performance}
			awk -- '{ print($1); }' tmp >> ${output_performance}
		done

		printf "Min: $min \t Max: $max \t Mean: $(echo "$sum / $nRun" |bc)\n"
	  printf "Min: $min \t Max: $max \t Mean: $(echo "$sum / $nRun" |bc)\n" >> ${output_performance}

	done
done
