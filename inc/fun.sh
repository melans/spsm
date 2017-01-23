################################################################################
function vars {
	echo -n "Welcome to Streamflow Prediction Statistical Model (SPSM)";counter;
	# vars
	# sites="08205500,08151500,08194500,09498500,05420500";
	sites="05420500";
	# start with Dec of startDT-1 to Dec endDT
	startDT="1958";
	endDT="2015";
	grid="4";	#	grid + / -
	Sites="Sites";	#	data folder
	# Correlation testing period
	yrs101="1989";
	yrs102="2000";
	# Prameters: decalre the array globally (-Ag)
	declare -Ag parameter;
		# parameter["cld"]="total cloud cover";
		# parameter["evap"]="evaporation";
		# parameter["gz"]="geopotential height";
		# parameter["lh"]="surface latent heat flux";
		# parameter["nlwg"]="net longwave at ground";
		# parameter["nswg"]="net shortwave at ground";
		# parameter["nswt"]="net shortwave at toa";
		# parameter["olr"]="outgoing longwave radiation";
		# parameter["omg"]="vertical pressure velocity";
		parameter["prec"]="total precipitation";
		# parameter["q"]="specific humidity";
		# parameter["sh"]="surface sensible heat flux";
		# parameter["slp"]="sea level pressure";
		parameter["st"]="surface temperature";
		# parameter["T"]="Forecast Time";
		# parameter["t2m"]="temperature at 2m";
		# parameter["taux"]="surface zonal wind stress";
		# parameter["tauy"]="surface meridional wind stress";
		# parameter["temp"]="temperature";
		# parameter["u"]="zonal velocity";
		# parameter["v"]="meridional velocity";

	XYURI="http://waterservices.usgs.gov/nwis/site/?format=rdb&sites=";
	mapURI="http://water.usgs.gov/wsc/cat/"
	flowURI="http://waterservices.usgs.gov/nwis/dv/?format=rdb&statCd=00003&parameterCd=00060&startDT=$[startDT-1]-12-01&endDT=$endDT-12-31&sites=";
	# echo $flowURI;exit
	gridsURI="http://iridl.ldeo.columbia.edu/SOURCES/.IRI/.FD/.ECHAM4p5/.Forecast/.ca_sst/.ensemble24/.MONTHLY/[M]average/S/(0000%201%20Jan%20$startDT)/(0000%201%20Dec%20$endDT)/RANGEEDGES";
	# _gridsURI="[M]average/gridtable.tsv";

	# dirs="dnld,temp,data,results,analysis";
	# dirs=(${dirs//,/ })
	# for dir in ${!dirs[@]}; do echo $dir.${dirs[$dir]};done
	# mkdir -p $Sites;
	# 0.dnld
	# 	info (Lat,Lon), map, downloads (flow, $p)
	# 1.temp
	#
	# 2.data
	# 	processed (Y.m, X.m.L, CPT files)
	# 3.CPT
	# 	CV.m.L, RP.m.L, CPT.out, CPT.err
	# 4.results
	#
	# 5.analysis
	# 	pdf
	M=(Months Jan Feb Mar Apr May Jun Jul Aug Sep Oct Nov Dec Jan Feb Mar Apr May Jun Jul Aug Sep Oct Nov Dec)
}
################################################################################
function _msg_ {
	# msgs
	counter;
	text="$1";delay="$2";if [ -z $delay ]; then delay=".01"; fi
	echo -en "\n";
	for i in $(seq 0 $(expr length "${text}")); do printf "${text:$i:1}";sleep ${delay};done;
}
################################################################################
function varSite {
	declare -Ag sitename["$1"]=`awk -F\\\t 'END{print $3}' $Sites/$1/0.dnld/latlon.info`
}
################################################################################
function varXY {
	eval `awk -F\\\t 'END{print "site_"$2"=("$6,$5,$NF"); "}' $Sites/$1/0.dnld/latlon.info`;
	X="site_$1[0]";X=${!X};X1=`echo $X-$grid|bc`;X2=`echo $X+$grid|bc`;
	Y="site_$1[1]";Y=${!Y};Y1=`echo $Y-$grid|bc`;Y2=`echo $Y+$grid|bc`;
}
################################################################################
function counter {
	echo -en "\r											..... $count";count=$[$count+1];
}
################################################################################
function downloading {
	_msg_ "Step 1: Downloading the data for sites ($sites)"
	for site in ${sites//,/ }; do
		counter;
		mkdir -p $Sites/$site/0.dnld $Sites/$site/1.temp $Sites/$site/2.data $Sites/$site/3.cpt  $Sites/$site/4.results $Sites/$site/5.analysis;
		wget $XYURI$site -cqO $Sites/$site/0.dnld/latlon.info > /dev/null;
		varSite $site;
		_msg_ "	Preparing the data for site # $site (${sitename[$site]}):";
		varXY $site;
		_msg_ "	Downloading the site's map";
		map="site_$site[2]";map=${!map};
		wget "$mapURI/$map.jpg" -cqO $Sites/$site/0.dnld/map_$site.jpg > /dev/null &
		_msg_ "	Downloading the site's streamflow data";
		wget "$flowURI$site" -cqO $Sites/$site/0.dnld/flow.0 > /dev/null &
		_msg_ "	Downloading data for ..."
		for p in "${!parameter[@]}"; do
			_msg_ "		 ${parameter[$p]} ($p)";
			tmpURI=$gridsURI/.$p/X/$X1/$X2/RANGEEDGES/Y/$Y1/$Y2/RANGEEDGES;
			for L in {1..7}; do
				_msg_ "			 $L months lead time ";
				LtmpURI=$tmpURI/L/$L/VALUE
				if [ $p == "gz" ] || [ $p == "omg" ] || [ $p == "q" ] || [ $p == "temp" ]; then
					for Z in 1000 0925 0850 0775 0700 0600 0500 0450 0400 0350 0300 0250 0200 0150 0100 0050 0025; do
						counter;
						wget "$LtmpURI/Z/$Z/VALUE/gridtable.tsv" -cqO $Sites/$site/0.dnld/$p.$L.$Z.0 > /dev/null;
					done
				else
					counter;
					wget "$LtmpURI/gridtable.tsv" -cqO $Sites/$site/0.dnld/$p.$L.0.0 > /dev/null;
				fi
			done
		done;echo;
	done;wait;
}
################################################################################
function formatting {
	_msg_ "Step 2: Formatting the data for sites ($sites)"
	for site in ${sites//,/ }; do
		counter;
		varSite $site;
		varXY $site;
		_msg_ "    Processing site # $site (${sitename[$site]}):";
		_msg_ "	Formatting : (year,month,day,value)";
		awk '$2=="'$site'"{split($3,dt,"-");print dt[1],dt[2],dt[3],$4}' $Sites/$site/0.dnld/flow.0 > $Sites/$site/1.temp/flow.1;
		_msg_ "	Monthly average : (year,month,avg)";
		awk '{sum[$1"-"$2]+=$4;d[$1"-"$2]+=1}END{for(i in d){split(i,dt,"-");print dt[1],dt[2],sum[i]/d[i]}}' $Sites/$site/1.temp/flow.1 | sort -nk1,2 > $Sites/$site/1.temp/flow.2;
		_msg_ "	Creating predictands (Y.00 - Y.12) files	";
		# create Y base file 1st col
		awk '{if($2==1)print $1}' $Sites/$site/1.temp/flow.2 > $Sites/$site/1.temp/_Y;
		# add header for the base file
		sed -i '1iSTN,\nLAT,\nLON,' $Sites/$site/1.temp/_Y;
		# create the previous yr Dec file (00) from Dec months and delete the last line
		awk '{if($2==12)print $3}' $Sites/$site/1.temp/flow.2 | sed '$d' > $Sites/$site/1.temp/_00;
		# create the months (01-12) files
		awk '{print $3>"'$Sites'/'$site'/1.temp/_"$2}' $Sites/$site/1.temp/flow.2;
		# remove 1st line in Dec file (which is Dec of prev yr)
		sed -i '1d' $Sites/$site/1.temp/_12;
		# add the header for months files (00-12 but not Y)
		sed -i '1iMM\n'$Y'\n'$X'' $Sites/$site/1.temp/_??;
		# put the month in the header
		for m in {00..12}; do
			counter;
			sed -i 's/MM/'$m'.0/' $Sites/$site/1.temp/_$m;
			# merge _Y and _mm to form Y.mm
			pr -mts" " $Sites/$site/1.temp/_Y $Sites/$site/1.temp/_$m > $Sites/$site/2.data/Y.$m.0;
			sed -i 's/, /,/' $Sites/$site/2.data/Y.$m.0;
		done
		# Fix missing Sep 2009 for site 08194500
		if [ $site == "08194500" ]; then
			_msg_ "		Fixing Sep 2009 for site #$site";
			sed -i '/2009 -999/d' $Sites/$site/2.data/Y.09.0;
			sed -i '/2008 /a 2009 -999' $Sites/$site/2.data/Y.09.0;
		fi
		# compose the X files
		for f in `ls $Sites/$site/0.dnld/*.?.*.0`; do
			counter;
			# fix parameters file date format
			awk 'NR>2{r=FILENAME;t=mktime("2000 "$3*1+1" 1 0 0 0");y=strftime("%Y",t)-40;m=strftime("%m",t);gsub(/0.dnld/,"1.temp",r);split(r,p,".");gsub(/0.dnld/,"1.temp",r);split(r,p,"/1.temp/");split(p[2],p,".");gsub(/1.temp\/\w+./,"1.temp/"p[1]"."m".",r);gsub(/.0$/,".1",r);print y,$2,$1,$4>r}' $f
		done
		# remove 1st line in Dec file (which is Dec of prev yr)
		# sed -i '1d' $Sites/$site/1.temp/*.?.*.12.1;
		_msg_ "	Creating Partial Predictors (X) files for : ...	";
		xp="";
		for f in `ls $Sites/$site/1.temp/*.??.?.*.1`; do
			counter;
			f2="${f/%.1/.2}";
			p=`echo $f|cut -d/ -f 4|cut -d. -f 1`;
			if [ $p != "$xp" ]; then
				_msg_ "		X($p)";
				xp=$p;
			fi;
			awk '{if(d==0 || d!=$1){Y=$2",";X=$3",";d=$1;p=1}else{Y=Y$2",";X=X$3",";p++}}END{for(i=1;i<=p;i++)printf "%s_%s,","'$p'",i;printf "\n%s\n%s",Y,X}' $f > $f2
			awk '{if(d==$1){printf "%s ",$4}else{printf "\n%s ",$4;d=$1};}' $f >> $f2
		done;echo;
	done;wait;
}
################################################################################
################################################################################

################################################################################
vars;
