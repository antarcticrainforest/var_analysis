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

echoerr() {
  echo "$@" 1>&2
  exit 257
}

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
    if [  "$(which idl)" ];then
      idl_cmd='idl'
    else
      idl_cmd='gdl'
    fi
    ${idl_cmd} <<EOF
    .r sub.pro
    .r interpolate_model.pro
    exit
EOF
    cd ${old_dir}
    ${workdir%/}/3D_create/create_hume_format/create_hume_data ${output%/}/3D_put ${output%/}/3D_put
    if [ $? -ne 0 ];then
      echoerr "3D_create/create_hume_format/create_hume_data had an error, aborting"
    fi

}

get_micro_input(){

   mkdir -p "${workdir%/}/process_MWR/temp"

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
   source ${workdir%/}/meta_data.conf
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
       sed "s%last=XXX%last=[${last}]%g"|\
       sed "s%smet_vprecip%$smet_vprecip%g"|\
       sed "s%smet_vtemp%$smet_vtemp%g"|\
       sed "s%smet_vrh%$smet_vrh%g"|\
       sed "s%smet_vp%$smet_vp%g"|\
       sed "s%smet_vu%$smet_vu%g"|\
       sed "s%smet_vd%$smet_vd%g" > tmp.pro
       #sed "s%readvars=['base_time','time_offset','precip_mean','temp_mean','relh_mean','lo_wind_spd_vec_avg','lo_wind_dir_vec_avg','atmos_pressure']%readvars=['base_time','time_offset','org_precip_rate_mean','temp_mean','rh_mean','wspd_vec_mean','wdir_vec_mean','atmos_pressure']%g"|\
    chmod +x tmp.pro
       mwr_vprecip=$(echo $smet_vprecip | tr [a-z] [A-Z])
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
       sed "s%mwr_vva%$mwr_vva%g"|\
       sed "s%mwr_vli%$mwr_vli%g"|\
       sed "s%mwr_vwet%$mwr_vwet%g"|\
       sed "s%mwr_vprecip%$mwr_vprecip%g"|\
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
    spawn, "mv ${output%/}/MWR-DATA/*.asc ${output%/}/MWR-DATA/ascii_out/"
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


}

rain_loop(){
    base_date=$1
    proc=$2
    dates=${@:3}
     for d in $(echo ${dates}|sed 's/,/ /g');do
        #Do the averaging
        ${workdir%/}/process_rain/create_dom_avg_pdf ${raininput%/} ${rainformat} ${workdir%/}/process_rain/ $proc $base_date ${d}
        if [ $? -ne 0 ];then
          echoerr "$proc : get_rain had an error, aborting"
        fi
    done
}

get_rain_input(){

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
    let nproc=$(get_num_process)
    #let nproc=1
    ary_split=$(python2 ${workdir%/}/split.py $nproc ${dates[*]})
    let proc=1
    for a in ${ary_split[*]};do
      #Loop through all threads and distribute the dates
      rain_loop $base_date $proc $a &
      let proc=${proc}+1
    done
    wait
    ndate=$(date -u -d "$(echo ${DATES[0]}|sed 's/_/ /')" +%s)
    last=$(date -u -d "$(echo ${DATES[1]}|sed 's/_/ /')" +%s)
    while [ $ndate -le $last ];do
      tstring=$(date -u -d @${ndate} +'%Y%m%d_%H%M')
      file=$(find ${raininput%/}/new/domain_avg_6hr/*${tstring}*.nc 2> /dev/null)
      if [ -z "$file" ];then
        python2.7 ${workdir%/}/process_rain/clone.py ${raininput%}/new/domain_avg_6hr $tstring
        if [ $? -ne 0 ];then
          echoerr "clone.py for $tstring failed"
        fi
      fi
      ndate=$(($ndate+21600)) #Go forward for 6H
    done
    #mkdir -p ${output%}/radar_rain
    # Create the rain ensemble time series
        ${workdir%/}/process_rain/create_timeseries \
        ${raininput%/}/new/domain_avg_6hr ${output%/}/radar_rain \
        ${array[0]} 0000 ${array[${#array[@]} - 1]} 2300 ${base_date} 6
    #rm -rf ${raininput%/}/new
}

get_num_process(){
    # This function is used to get the optimal number of processes that can 
    # be used at once
    
    let nproc=1 #default number of threads
    os=$(uname -s)
    if [ "$os" == "Linux" ];then
        let nproc=$(fgrep processor /proc/cpuinfo|wc -l)
    elif [ "$os" == "Darwin" ];then
        let nproc=$(system_profiler | awk '/Number Of CPUs/{print $4}{next;}'})
    fi
    if [ $nproc -ge 8 ];then
        let nproc=8
    fi
    echo $nproc
}
#####Get everything we need for running the create_2d_input_files script
#get_2d_input 


###########################################
#              FOR DEBUGGING              #
input="$HOME/Data/ARM/0506/"
raininput="$HOME/Data/CPOL/0506/"
rainformat='nc'
output="$HOME/Data/var_ana/va_inputs/0506/"
filename="ecmwf.nc"
workdir=$(dirname $(readlink -f $0))
va_output="$HOME/Data/var_ana/va_output/0506"
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
#Get the start and end date of the wet season (Radar rain availability)
#DATES=$(python2 get_dates.py $raininput)
DATES="20060201_0000 20060228_1800 20060201" #This is for debugging only
IFS=' ' read -a DATES <<< "$DATES" #Make those dates an array
#Call the create_2d_input_files script
mkdir -p ${output}
mkdir -p ${va_output}
#${workdir}/2D_create/create_2d_input_files $input ${output%/}/2D_put $filename ${DATES[*]}
if [ $? -ne 0 ];then
  echoerr "create_2d_input_files had an error, aborting"
fi
#####Get the 3d_data
#${workdir}/3D_create/create_netcdf/concatenate_arm_data $input ${output%/}/3D_put ${DATES[*]}
if [ $? -ne 0 ];then
  echoerr "concatenate_arm_data had an error, aborting"
fi
#get_3d_input
if [ $? -ne 0 ];then
  echoerr "get_3d_input had an error, aborting"
fi


#####Get the microwave input data
#get_micro_input 'smet' 'mwrlos' ${DATES[*]}
if [ $? -ne 0 ];then
  echoerr "get_micro_input had an error, aborting"
fi


####Prepare the raindata
#get_rain_input
if [ $? -ne 0 ];then
  echoerr "get_rain_input had an error, aborting"
fi


echo 'Preprocessing done, running variational analysis'

${workdir%/}/process.sh ${output} ${va_output}
if [ $? -ne 0 ];then
  echoerr "3D VAR had an error, aborting"
fi


