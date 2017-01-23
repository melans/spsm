#!/bin/bash

#SBATCH -J SPSM			# job name
#SBATCH -o SPSM.%j.out		# output and error file name (%j expands to jobID)
#SBATCH -e SPSM.%j.err		# output and error file name (%j expands to jobID)
#SBATCH -n 256				# total number of mpi tasks requested
#SBATCH -p normal			# queue (partition) -- normal, development, etc.
#SBATCH -t 10:00:00			# run time (hh:mm:ss) - 1.5 hours
#SBATCH --mail-user=anssary@gmail.com
#SBATCH --mail-type=ALL

. inc/fun.sh

################################################################################
function CPT {
	counter;
	cd 11.03;
	./CPT.x < ../$1 > ../$1.out;
	cd ..;
}
################################################################################
# X_composing site parameters Lead_time Z month
# ex.	(X_composing 05420500 "prec st" 1 01 0;)
function predicting_Ls {
	counter;
	# take site, parameter, lead time, Z, and month,and generate X.p.L.Z.m from (_mm , p.L.Z.m.2 , _Y)
	site=$1;ps=$2;m=$3;L=$4;Z=$5;
	m0=`echo $(printf %02d $[10#$m-1])`;
	x="";xx="";
	_msg_ "		... ${M[10#$m+$L-1]} ($L month(s) lead time)"
	for p in ${ps//,/ }; do
		counter;
		x=$x"$Sites/$site/1.temp/$p.$m.$L.$Z.2 ";
		xx=$xx$p"_";
	done;
	xx="${xx/%_/}";
	x=$x"$Sites/$site/1.temp/_$m";
	cpt=$Sites/$site/3.cpt/$xx.$m.$L.$Z.cpt;
	pr -mts' ' $Sites/$site/1.temp/_Y $x|sed 's/, /,/g' > $Sites/$site/2.data/X.$xx.$m.$L.$Z;
	X=../$Sites/$site/2.data/X.$xx.$m.$L.$Z;
	if [ $L -eq 1 ]; then
		Y=../$Sites/$site/2.data/Y.$m0.$[L-1];
	else
		Y=../$Sites/$site/2.data/Y.$m.$[L-1];
	fi;
	CV=../$Sites/$site/3.cpt/$xx.$m.$L.$Z.CV;
	RP=../$Sites/$site/3.cpt/$xx.$m.$L.$Z.RP;
	limits=`awk 'NR>2{if(l1<$2||!l1)l1=$2;if(l2>$2||!l2)l2=$2;if(l3>$1||!l3)l3=$1;if(l4<$1||!l4)l4=$1;}END{print l1,l2,l3,l4}' $Sites/$site/0.dnld/$p.1.0.0`
	awk '{print $1}' inc/CPT.conf|\
		sed 's/X/'${X//\//\\/}'/g'|\
		sed 's/Y/'${Y//\//\\/}'/g'|\
		sed 's/Limits/'${limits// /\\n}'/g'|\
		sed 's/CV/'${CV//\//\\/}'/g'|\
		sed 's/RP/'${RP//\//\\/}'/g' \
	> $cpt;
	CPT $cpt;
	Q0=$Sites/$site/2.data/Y.$m.$[L-1]
	Q=$Sites/$site/2.data/Y.$m.$L;
	# echo "STN,$m,$l" > $Q;
	# head -n3 $Q0|sed  >> $Q;
	head -n3 $Q0 > $Q;
	awk 'NR>6{print $1,$2}' $Sites/$CV.txt >> $Q;
}
################################################################################
function predicting {
	counter;
	# take site, parameter, lead time, Z, and month,and generate X.p.L.Z.m from (_mm , p.L.Z.m.2 , _Y)
	site=$1;ps=$2;x="";xx="";
	_msg_ "Step 3: Run CPT on site ($site) at ..."
	for m in {01..12}; do
		_msg_ "	... ${M[10#$m]} to predict streamflow of ..."
		for L in {1..7}; do
			counter;
			predicting_Ls $site "$ps" $m $L 0;
		done;
		# predicting_Ls 05420500 "prec st" $m 1 0;
	done;
	# predicting_Ls 05420500 "$ps" 01 1 0;

}
################################################################################
function calcs {

	yy=`awk -vORS=, '$1>1986&&$1==$1*1{print $2}' $Sites/$site/2.data/Y.$m.0|sed 's/,$//g'`;
	cv=`awk -vORS=, '$1>1986&&$1==$1*1{print $2}' $Sites/$site/3.cpt/$xx.$m.$L.$Z.CV.txt|sed 's/,$//g'`;
	rp=`awk -vORS=, 'NR>6{print $2}' $Sites/$site/3.cpt/$xx.$m.$L.$Z.RP.txt|sed 's/,$//g'`
	Rscript -e 'cat(",",cor(c('$yy'),c('$rp'),use="pairwise.complete.obs",method="spearman"))'|sed 's/\[1\] //g';
}
################################################################################

# CPT 05420500 X.prec_st.1.0.01 Y.00
# 05420500 "prec st" 1 01 0;


# exit






function analysing {
	echo;
	#statements
}
################################################################################
# downloading;
# formatting;


# echo;
# X_composing 05420500 prec 1 0 01;
# predicting 05420500 "prec st" 1 01 0;
# predicting 05420500 "prec st";

# module load R;
# correlating;wait;
_msg_ "DONE";
counter;
echo;

################################################################################
# 05420500 Mississippi River at Clinton, IA
# 08151500 Llano Rv at Llano, TX
# 08194500 Nueces Rv nr Tilden, TX
# 08205500 Frio Rv nr Derby, TX
# 09498500 SALT RIVER NEAR ROOSEVELT, AZ
# flow				http://waterservices.usgs.gov/nwis/dv/?format=rdb&statCd=00003&parameterCd=00060&startDT=$startDT&endDT=$endDT&sites=
# lon(X),lat(Y)		"http://waterservices.usgs.gov/nwis/site/?format=rdb&sites=08205500,08151500,08194500,09498500,05420500"
# map							"http://water.usgs.gov/wsc/cat/$huc_cd.jpg"
#
# http://iridl.ldeo.columbia.edu/SOURCES/.IRI/.FD/.ECHAM4p5/.Forecast/.ca_sst/.ensemble24/.MONTHLY/_PARAMETER_/X/_X1_/_X2_/RANGE/Y/_Y1_/_Y2_/RANGE/%5BM%5Daverage/gridtable.tsv
# http://iridl.ldeo.columbia.edu/SOURCES/.IRI/.FD/.ECHAM4p5/.Forecast/.ca_sst/.ensemble24/.MONTHLY/.prec/Y/45/37/RANGE/X/-94/-86/RANGE/-99999.0/setmissing_value/%5BM%5Daverage/%5BX/Y%5D%5B%5Dcptv10.tsv
# http://iridl.ldeo.columbia.edu/SOURCES/.IRI/.FD/.ECHAM4p5/.Forecast/.ca_sst/.ensemble24/.MONTHLY/.prec/Y/45/37/RANGE/X/-94/-86/RANGE/-99999.0/setmissing_value/%5BM%5Daverage/%5BS%5Daverage/%5BX/Y%5D%5B%5Dcptv10.tsv
#
# my_dir="$(dirname "$0")";"$my_dir/other_script.sh"
################################################################################
