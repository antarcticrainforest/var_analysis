#!/bin/bash

# This bash script processes the ecmwf-data to be pre-processed
# It takes the Data-directories containing the ECMWF-ARM site data
#
# as a prameter and calls three different functions:
#
# dates() :
#       gets the dates between start and end period
# create_2d_input():
#       calls the create_2d_input_files ksh script
#
#
#
# Ok let's go:
# Where was the data from the ARM-FTP-Server downloaded?



get_2d_input () {

    while true;do
        echo -n -e "${err}What is the ecmwf input folder:   "
        read input
        if [ -z "$input" ];then
            err="Error while reading ecmwf input  .. "
        elif [ ! -d "${input}" ];then
            err="Error path not existing  .. "
        else
            break
        fi
    done

    while true;do
        echo -n -e "${err}What is the rain input folder:   "
        read raininput
        if [ -z "$raininput" ];then
            err="Error while reading rain input  .. "
        elif [ ! -d "${input}" ];then
            err="Error path not existing  .. "
        else
            break
        fi
    done


    while [ -z "$output" ];do
        echo -n "${err}What is the output folder:   "
        read output
        err="Error while reading input  .. "
    done

    err=
    while [ -z "$filename" ];do
        echo -n -e "${err}What is the filename of the output:   "
        read filename
        err="Error while reading input  .. "
    done
            
    if [ "${output[${#output[@]} - 1]}" != "/" ];then
        output="$output/"
    fi
 
    if [ "${input[${#input[@]} - 1]}" != "/" ];then
        input="$input/"
    fi
 
    echo -e -n "The following configuration will be passed: \n \
        \t input  dirname  : $input \n \
        \t output dirname  : $output \n \
        \t output filename : $filename \n \
        \n
        \t is this configuration correct? [Y]"
    read correct

    if [ "${correct[1]}" == "n" ] || [ "${correct[1]}" == "N" ];then
        get_2d_input
    fi

    if [ ! -d ${output} ];then
        mkdir -p ${output}
    fi



}


get_3d_input(){
    #Where is the interpolation idl/gdl-script stored
    barnes="${workdir}/3D_create/Barnes_interp/.interpolate_model.pro"

    #Adjust the idl-script according to the output
    cat ${barnes} | sed "s%inputr1='XXXX'%inputr1='${output%/}/3D_put/upa_p.nc'%g" \
        | sed "s%inputr2='XXXX'%inputr2='${output%/}/3D_put/surf_p.nc'%g"\
        | sed "s%output3d='XXXX'%output3d='${output%/}/3D_put/analysis.agrid'%g"\
        | sed "s%dir_out='XXXX'%dir_out='${output%/}/3D_put/'%g" > tmp.pro
    
    chmod +x tmp.pro
    barns="${workdir%/}/3D_create/Barnes_interp"
    mv tmp.pro "${barns%/}/interpolate_model.pro"
    old_dir=$PWD
    
    #Now run the gdl-script
    cd ${barns}
    gdl <<EOF
    .r sub.pro
    .r interpolate_model.pro
    exit
EOF
    cd ${old_dir}
    ${workdir%/}/3D_create/create_hume_format/create_hume_data ${output%/}/3D_put ${output%/}/3D_put
    ${workdir}/3D_create/create_hume_format/create_hume_data ${output%/}/3D_put ${output%/}/3D_put



}

get_micro_input(){

   mkdir -p "${workdir%s}/process_MWR/temp"

#   IFS=' ' read -a DATES <<< "$DATES"
    declare -a dates=${@:3}
    python2 ${workdir%/}/process_MWR/date.py $dates
    mapfile -t  years_str <  ${workdir%/}/process_MWR/.years
    mapfile -t months_str < ${workdir%/}/process_MWR/.months
    mapfile -t first < ${workdir%/}/process_MWR/.first
    mapfile -t last < ${workdir%/}/process_MWR/.last
    
    echo ${months_str[*]}|sed "s/'//g" > ${workdir%/}/process_MWR/.months
    echo ${years_str[*]}|sed "s/'//g" > ${workdir%/}/process_MWR/.years

    mapfile -t  years_int <  ${workdir%/}/process_MWR/.years
    mapfile -t  months_int <  ${workdir%/}/process_MWR/.months
    
    fy=$(echo ${years_int}|cut -d , -f1)
    fm=$(echo ${months_int}|cut -d , -f1 )
    fd=$(echo ${first}|cut -d , -f1)
   init_time="julday(${fm},$fd,${fy},0,0,0)-0.5"
   init_time2="julday(${fm},1,${fy},0,0,0)-0.5"
   base_date="${fy}${fm}${fd}"
    cat ${workdir%/}/process_MWR/.process_${1}_a1_darwin.pro| \
       sed "s%@WRKDIR/%@${workdir%/}/%g" |\
       sed "s%path_prefix='WRKDIR'%path_prefix='${workdir%/}/'%g" |\
       sed "s%data_path='INPUT'%data_path='${input%/}/'%g"|\
       sed "s%file_id='ID'%file_id='met'%g"|\
       sed "s%out_dir='OUTPUT'%out_dir='${output%/}/MWR-DATA/'%g"|\
       sed "s%years_int=XXX%years_int=[${years_int}]%g" |\
       sed "s%years_str=XXX%years_str=[${years_str}]%g" |\
       sed "s%months=XXX%months=[${months_str}]%g" |\
       sed "s%ini_time=XXX%ini_time=${init_time}%g"|\
       sed "s%ini_time2=XXX%ini_time2=${init_time2}%g"|\
       sed "s%first=XXX%first=[${first}]%g"|\
       sed "s%last=XXX%last=[${last}]%g"> tmp.pro
       #sed "s%readvars=['base_time','time_offset','precip_mean','temp_mean','relh_mean','lo_wind_spd_vec_avg','lo_wind_dir_vec_avg','atmos_pressure']%readvars=['base_time','time_offset','org_precip_rate_mean','temp_mean','rh_mean','wspd_vec_mean','wdir_vec_mean','atmos_pressure']%g"|\
    chmod +x tmp.pro
    seas=$(echo ${output}|rev|cut -d / -f2 |rev)
    mv tmp.pro ${workdir%/}/process_MWR/process_${1}_a1_darwin.pro
    cat ${workdir%/}/process_MWR/.process_${2}_a1_darwin.pro| \
       sed "s%@WRKDIR/%@${workdir%/}/%g" |\
       sed "s%path_prefix='WRKDIR'%path_prefix='${workdir%/}/'%g" |\
       sed "s%data_path='INPUT'%data_path='${input%/}/'%g"|\
       sed "s%file_id='ID'%file_id='${2}'%g"|\
       sed "s%out_dir='OUTPUT'%out_dir='${output%/}/MWR-DATA/'%g"|\
       sed "s%years_int=XXX%years_int=[${years_int}]%g" |\
       sed "s%years_str=XXX%years_str=[${years_str}]%g" |\
       sed "s%months=XXX%months=[${months_str}]%g" |\
       sed "s%ini_time=XXX%ini_time=${init_time}%g"|\
       sed "s%ini_time2=XXX%ini_time2=${init_time2}%g"|\
       sed "s%hourly_path='INPUT'%hourly_path='${output%/}/MWR-DATA/ascii_out/'%g"|\
       sed "s%ascii_path='OUTPUT'%ascii_path='${output%/}/MWR-DATA/ascii_out/'%g"|\
       sed "s%OUTPUT=XXX%OUTPUT='${output%/}/MWR-DATA/'%g"|\
       sed "s%seas=XXX%seas='${seas}'%g"|\
       sed "s%XXbase_timeXX%${base_date}%g"|\
       sed "s%first=XXX%first=[${first}]%g"|\
       sed "s%last=XXX%last=[${last}]%g"> tmp.pro

    mkdir -p ${output%/}/MWR-DATA/Plot
    mkdir -p ${output%/}/MWR-DATA/ascii_out
    chmod +x tmp.pro
    mv tmp.pro ${workdir}/process_MWR/process_${2}_a1_darwin.pro
    old_dir=$PWD
    cd ${workdir}/process_MWR
    gdl <<EOF
    .r process_${1}_a1_darwin.pro
    .r process_${2}_a1_darwin.pro
    exit
EOF

    units=$(ncdump -h ${output%/}/2D_put/${filename}|grep 'time:units'|cut -d = -f2|sed 's/;//'|sed 's/^ *//'|sed 's/\"//g'|sed 's/[ \t]*$//g')
    units="days since $fy-$fm-$fd 00:00:00 UTC"
    if [ -f "${output%/}/MWR-DATA/mwrlos_6h_${seas}_interp3.nc" ];then
        rm ${output}mwrlos_6h_${seas}_interp3.nc
    fi
    
    mv  ${output%/}/MWR-DATA/mwrlos_6h_${seas}_C3_interp3.nc ${output%/}/MWR-DATA/mwrlos_6h_${seas}_interp.nc
    ncatted -a units,time,o,c,"$units" ${output%/}/MWR-DATA/mwrlos_6h_${seas}_interp.nc


    mv ${output%/}/MWR-DATA/*.asc ${output%/}/MWR-DATA/ascii_out/
}

 
get_rain_input(){

    echo 'start rain input'

    cmd=$(which gdate 2> /dev/null)
    if [ -z "$cmd" ];then
        cmd=$(which date)
    fi
    DATES_LAST=$(echo ${DATES[1]}|cut -d _ -f1)
    DATES_FIRST=$(echo ${DATES[0]}|cut -d _ -f1)
    dates=$(python2 -c "from datetime import datetime, timedelta as td;\
        d1,d2=datetime.strptime('${DATES_FIRST}','%Y%m%d'),\
        datetime.strptime('${DATES_LAST}','%Y%m%d');\
        dt=d2-d1;d=[(d1+td(days=i)).strftime('%Y%m%d') for i in xrange(dt.days+1)];\
        print ' '.join(d).strip('\n')")
    
    python2 ${workdir%/}/process_rain/mask.py #${workdir%/}/process_rain/tmp.nc ${workdir%/}/process_rain/mask.nc
    #Get the number of max. threads
    IFS=' ' read -a array <<< "$dates"
    base_date=$(python2 -c "from datetime import datetime;\
        d1=datetime.strptime('${array[0]}','%Y%m%d');\
        print d1.strftime('%Y%m01')")
    #echo $base_date
    #let nproc=$(get_num_process)
    let nproc=1
    #Loop through all dates and distribute the parallel threads
    for d in ${dates[*]};do
        #Do the averaging
        ${workdir%/}/process_rain/create_dom_avg_pdf ${raininput%/} \
            ${rainformat} ${workdir%/}/process_rain/ $base_date ${d} #&
        if [ "$(jobs |wc -l)" == "${nproc}" ];then
            echo -n "NOW WAITING FOR THE ${nproc} JOBS TO FINISH ......"
        fi
        while [ $(jobs |wc -l ) -ge ${nproc} ]; do
            sleep 0.1
            jobs > /dev/null
        done
        if [ "$(jobs |wc -l)" == '0' ];then
            echo '  done'
        fi
       # rm -fr ${raininput%/}/new/*${d}*.nc
        rm -fr ${raininput%/}/new/domain_avg_10min/*${d}*.nc
        rm -fr ${raininput%/}/new/pdf_10min/*${d}*.nc
        rm -fr ${raininput%/}/new/pdf_6hr/*${d}*.nc
    
    done
    wait
    #mkdir -p ${output%}/radar_rain
    # Create the rain ensemble time series
        ${workdir%/}/process_rain/create_timeseries \
        ${raininput%/}/new/domain_avg_6hr ${output%/}/radar_rain \
        ${array[0]} 0000 ${array[${#array[@]} - 1]} 2300 ${base_date} 6
    rm -rf ${raininput%/}/new
}

get_num_process(){
    # This function is used to get the optimal number of processes that can 
    # be used at once
    
    nproc=1 #default number of threads
    os=$(uname -s)
    if [ "$os" == "Linux" ];then
        nproc=$(fgrep processor /proc/cpuinfo|wc -l)
    elif [ "$os" == "Darwin" ];then
        nproc=$(system_profiler | awk '/Number Of CPUs/{print $4}{next;}'})
    fi

    echo $nproc
}
#####Get everything we need for running the create_2d_input_files script
#get_2d_input 


###########################################
#              FOR DEBUGGING              #
input="/home/ljun0002/Data/ARM/0506/"
raininput="/home/ljun0002/Data/CPOL/0506/"
rainformat='nc'
output="/home/ljun0002/Data/var_ana/va_inputs/0506/"
filename="ecmwf.nc"
workdir=$(dirname $(readlink -f $0))
va_output="/home/ljun0002/Data/var_ana/va_output/0506"
#
# Read the command line.
#
while [[ $# -ge 1 ]]
do
	typeset -l option="${1}"
  echo $option
	case "${option}" in
		( "-a" | "--arminput" )
		input="${2:-${input}}"
		shift; shift
		;;
		( "-r" | "--raininput" )
		raininput="${2:-${raininput}}"
		shift; shift
		;;
		( "-o" | "--output" )
		va_output="${2:-${va_output}}"
		shift; shift
		;;
		( "-v" | "--va_input" )
		output="${2:-${output}}"
		shift; shift
		;;
		( * )
		echo "E: Unknown option: ${1}"
    echo "Usage:   ${0} [OPTIONS]"|sed "s#./##g"
    echo "Options:"
    echo "-a , --arminput  : Input dir of the atmospheric data"
    echo "-r , --raininput : Input dir of the radar data"
    echo "-o , --output    : Output dir of the varational analysis"
    echo "-v , --va_input  : Input dir of the variational analysis"
		exit 2
		;;
	esac
done

##########################################
#####Get dates:
DATES=$(python2 get_dates.py $raininput)
DATES="20060201_0000 20060228_1200 20060201"
IFS=' ' read -a DATES <<< "$DATES"
#Call the create_2d_input_files script
mkdir -p ${output}
mkdir -p ${va_output}
${workdir}/2D_create/create_2d_input_files $input ${output%/}/2D_put $filename ${DATES[*]}
#####Get the 3d_data
${workdir}/3D_create/create_netcdf/concatenate_arm_data $input ${output%/}/3D_put ${DATES[*]}
#get_3d_input
#####Get the microwave input data
#get_micro_input 'smet' 'mwrlos' ${DATES[*]}
####Prepare the raindata
get_rain_input
echo 'exit'
exit

echo -e "Pre-processing done now. Do you want to run the following command:\n \n \
    \t ${workdir%/}/process.sh ${output} ${va_outut} [Y/n]\n"

for i in {1..4};do
  echo -n '.'
  sleep 1
done
echo '.'
if [ -z "${question}" ] || [ "${question}" == 'y' ] || [ "${question}" == 'Y' ] || [ "${question}" == 'yes' ] || [ "${question}" == 'Yes' ];then
    ${workdir%/}/process.sh ${output} ${va_output}
fi

