#!/usr/bin/env bash
#
###################################################################################
#
# This script reads surface variables from the ECMWF analysis data 
# in a format as from the ARM data archive
# (netcdf with the ARM time convention)
#
# The input files are named: 
#
# twpecmwfdarsuppX1.c1.yyyymm01.000000.cdf
# twpecmwfdarsurfX1.c1.yyyymm01.000000.cdf
#
# Currently each file has one month of data in it. 
# The twp region is a 7x7 grid at 0.5 deg resolution
#
# 
#
# It outputs a single netcdf file for the time period specified using the dates variable
# It averages the data in space over the gridpoints specified in the masking file 
# This is a crude average for an irregularly shaped domain, but given the uncertainties
# in the analysis itself this should be ok.
#
# Currently the temporal resolution of the data is not changed by this program
#
#
#
#
##################################################################################
#
# Options

# Set where relevant files are located
ipt_dir=${1%/}/
opt_dir=${2%}/

if [ ! -d ${opt_dir} ];then
    mkdir -p ${opt_dir}
fi

# Set the output filename
outfile=${3%/}

cmd=$(which gdate 2> /dev/null)
if [ -z "$date" ];then
    cmd=$(which date)
fi
if [ -z "$cmd" ];then
    echo "Can't find date or gdate, if you are on osx considere installing coreutils"
    exit 1
fi

# Set the times we require
date_begin="${4} 0000"
date_end="$($cmd -d "${@:(-1):1} 0000 + 1 month - 1 day" "+%Y%m%d 1200")"
# Names of all the months in between our two dates
declare -a dates=${@:4}

##################################################################################



# Set the full paths to the input and output directories
outfile=${opt_dir%/}/${outfile}
scriptdir=$(dirname $(readlink -f  $0))

var1="time,time_offset,base_time,sktsfc,spsfc,no10usfc,no10vsfc,no2tsfc,no2dsfc,lccsfc,mccsfc,hccsfc,zsfc,twcsfc,srsfc"
var2="ewsssfc,nssssfc,sshfsfc,slhfsfc,ssrsfc,strsfc,tsrsfc,ttrsfc,tccsfc,ssrdsfc,strdsfc"

i=0
for date in ${dates[*]}
do
 # Find the files required
   file_surf[$i]=$(find ${ipt_dir%/}/*surf*${date}*)
   file_supp[$i]=$(find ${ipt_dir%/}/*supp*${date}*)

   file_tmp[$i]="${date}_tmp.cdf"
  
 # get the variables we need from the surf and supp files
   ncks -h -O -v "${var1}" "${file_surf[i]}" "${file_tmp[i]}"
   ncks -h -A -v "${var2}" "${file_supp[i]}" "${file_tmp[i]}"

   i=$(($i+1))
done
# Set baseline dates and time cutoffs ########################################


base_date=${@:4:1}
base_time=$($cmd -u -d "${base_date}" +%s )


time_begin=$(($($cmd -u -d "${date_begin}" +%s ) - $($cmd -u -d "${base_date}" +%s )))
time_begin=$(echo $time_begin|awk '{printf("%.2f",$1/86400.0)}')
time_end=$(($($cmd -u -d "${date_end}" +%s ) - $($cmd -u -d "${base_date}" +%s )))
time_end=$(echo $time_end|awk '{printf("%.2f",$1/86400.0)}')

base_date=$($cmd -u -d @$(($base_time)) +%Y-%m-%d )


# Print some stuff to screen ###############################################s
echo " ************************************"
echo " "
echo "       Creating 2D_put file"
echo " "
echo " ************************************"
echo " "
echo "Time-series defined:"
echo "from time = $time_begin to $time_end days after $base_date"
echo " "

# concatenate files (nco knows about arm conventions in time_offset and base_time, but not in time itself
ncrcat -h -O "${file_tmp[@]}" ${outfile}

# fix the 'time' variable
ncap -h -O -s "time = time_offset/86400" ${outfile} ${outfile}
ncatted -h -a units,time,o,c,"days since ${dates[0]:0:4}-${dates[0]:4:2}-01 00:00:00 UTC" "${outfile}"

# make a masking file for the domain we require
${scriptdir%/}/make_mask_ecmwf -i "${outfile}" -o ${opt_dir%/}/mask.nc -d pentagon
# average over the gridpoints specified by the masking file.
# this is just a sum over the points inside the domain... nothing fancy.
ncks -h -A -v masking_var ${opt_dir%/}/mask.nc "${outfile}"
ncwa -h -O -a latitude,longitude -m masking_var -T gt -M -1.0 "${outfile}" "${outfile}"
# clean up and make like Tim's file ###########################

# remove unwanted var
ncks -h -O -x -v latitude,masking_var,base_time,longitude "${outfile}" "${outfile}"

# fix up baseline
ncap -h -A -s "bdate=${dates[0]}" "${outfile}" "${outfile}"
ncatted -h -a units,bdate,o,c,"yyyymmdd" "${outfile}"
ncatted -h -a long_name,bdate,o,c,"baseline date" "${outfile}"


# change from units * sec to the right ones

deltat=$(ncks -v time -d time,1,1,1 $outfile)
# At some stage I understood sed and wrote this. 
deltat=$(echo ${deltat}]} | sed 's/.*\(time[1]\)*\([0-9]\.[0-9]*[0-9]\).*/\2/')

deltat=$deltat
deltat_sec=$(echo $deltat|awk '{printf("%.2f",$1*86400)}')
echo "Temporal resolution: $deltat_sec seconds"

var1="time,time_offset,base_time,sktsfc,spsfc,no10usfc,no10vsfc,no2tsfc,no2dsfc,lccsfc,mccsfc,hccsfc,zsfc,twcsfc,srsfc"
var2="ewsssfc,nssssfc,sshfsfc,slhfsfc,ssrsfc,strsfc,tsrsfc,ttrsfc,tccsfc,ssrdsfc,strdsfc"


#script1="ewsssfc=ewsssfc/$deltat_sec"
#script2="nssssfc=nssssfc/$deltat_sec"

script3="sshfsfc=sshfsfc/$deltat_sec"
script4="slhfsfc=slhfsfc/$deltat_sec"
script5="ssrsfc=ssrsfc/$deltat_sec"
script6="strsfc=strsfc/$deltat_sec" 
script7="tsrsfc=tsrsfc/$deltat_sec" 
script8="ttrsfc=ttrsfc/$deltat_sec"

script9="ssrdsfc=ssrdsfc/$deltat_sec" 
script10="strdsfc=strdsfc/$deltat_sec"

script11="spsfc=spsfc/100"

vars=( ewsssfc nssssfc sshfsfc slhfsfc ssrsfc strsfc tsrsfc ttrsfc ssrdsfc strdsfc )

for v in ${vars[*]};do
    python -c "from netCDF4 import Dataset as nc;f=nc('${outfile}','a');f.variables['$v'][:]/=$deltat_sec;f.close()"
done
v=spsfc
python -c "from netCDF4 import Dataset as nc;f=nc('${outfile}','a');f.variables['$v'][:]/=100;f.close()"

#exit

#ncap -h -O -s $script1 $outfile $outfile
#ncap -h -O -s $script2 $outfile $outfile
#ncap -h -O -s $script3 $outfile $outfile
#ncap -h -O -s $script4 $outfile $outfile
#ncap -h -O -s $script5 $outfile $outfile
#ncap -h -O -s $script6 $outfile $outfile
#ncap -h -O -s $script7 $outfile $outfile
#ncap -h -O -s $script8 $outfile $outfile
#ncap -h -O -s $script9 $outfile $outfile
#ncap -h -O -s $script10 $outfile $outfile
#ncap -h -O -s $script11 $outfile $outfile



ncap -O -h -s "surf_net=ssrsfc+strsfc" "${outfile}" "${outfile}"


# Fix attributes

ncatted -h -a units,ssrsfc,o,c,"W/m2" "${outfile}"
ncatted -h -a long_name,ssrsfc,o,c,"SW net surface radiation" "${outfile}"

ncatted -h -a units,strsfc,o,c,"W/m2" "${outfile}"
ncatted -h -a long_name,strsfc,o,c,"LW net surface radiation" "${outfile}"

ncatted -h -a units,surf_net,o,c,"W/m2" "${outfile}"
ncatted -h -a long_name,surf_net,o,c,"total net surface radiation" "${outfile}"

ncatted -h -a units,ttrsfc,o,c,"W/m2" "${outfile}"
ncatted -h -a long_name,ttrsfc,o,c,"TOA long-wave radiation" "${outfile}"

ncatted -h -a units,tsrsfc,o,c,"W/m2" "${outfile}"
ncatted -h -a long_name,tsrsfc,o,c,"TOA short-wave radiation" "${outfile}"

ncatted -h -a units,ssrdsfc,o,c,"W/m2" "${outfile}"
ncatted -h -a long_name,ssrdsfc,o,c,"Surface solar radiation down" "${outfile}"

ncatted -h -a units,strdsfc,o,c,"W/m2" "${outfile}"
ncatted -h -a long_name,strdsfc,o,c,"Surface long-wave radiation down" "${outfile}"

ncatted -h -a units,ewsssfc,o,c,"Ns/m2" "${outfile}"
ncatted -h -a long_name,ewsssfc,o,c,"E-W surface stress" "${outfile}"

ncatted -h -a units,nssssfc,o,c,"Ns/m2" "${outfile}"
ncatted -h -a long_name,nssssfc,o,c,"N-S surface stress" "${outfile}"

ncatted -h -a units,sshfsfc,o,c,"W/m2" "${outfile}"
ncatted -h -a long_name,sshfsfc,o,c,"surface sensible heat-flux" "${outfile}"

ncatted -h -a units,slhfsfc,o,c,"W/m2" "${outfile}"
ncatted -h -a long_name,slhfsfc,o,c,"surface latent heat-flux" "${outfile}"

ncatted -h -a units,spsfc,o,c,"hPa" "${outfile}"


timeshift=1

if [[ $timeshift -gt 0 ]]
then

   # ok. Now need to move the flux data y three hours. This is nasty.



   # make two files of the desired time period. One 6 hours ahead of the other

   # some time variables
   tfl1=$(echo ${time_begin} ${deltat} |awk '{printf("%.2f",$1+$2)}')
   tfl2=$(echo ${time_end} ${deltat}|awk '{printf("%.2f",$1+$2)}')
   echo $tfl1,$tfl2
   ncks -h -O -d "time,$tfl1,$tfl2" ${outfile} "${outfile%.nc}_tmp.nc"

   tfl1=$(echo ${time_begin} | awk '{printf("%.2f",$1)}')
   tfl2=$(echo ${time_end} | awk '{printf("%.2f",$1)}')
   ncks -h -O -d "time,$tfl1,$tfl2" ${outfile} ${outfile}

   # line the two time regions up
   ncap -h -O -s "time = time - ${deltat}" "${outfile%.nc}_tmp.nc" "${outfile%.nc}_tmp.nc"


   # average the fluxes between t and t+dt
   var3="ewsssfc,nssssfc,sshfsfc,slhfsfc,ssrsfc,strsfc,tsrsfc,ttrsfc,ssrdsfc,strdsfc,surf_net"
   ncflint -h -O -v $var3 ${outfile} "${outfile%.nc}_tmp.nc" "${outfile%.nc}_tmp.nc"

   # put the averaged fluxes in the original file
   ncks -h -A "${outfile%.nc}_tmp.nc" ${outfile}

else
   ncks -h -O -d "time,${time_begin},${time_end}" ${outfile} ${outfile}

fi


# The variational analysis likes the names to be like this:
ncrename -h -O -v tsrsfc,swt -v ttrsfc,lwt ${outfile}


# remove temporary files
rm "${outfile%.nc}_tmp.nc"
rm *_tmp.cdf



