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

get_3d_input(){
    #Where is the interpolation idl/gdl-script stored
    barnes="${workdir}/3D_create/Barnes_interp/.interpolate_model.pro"

    #Adjust the idl-script according to the output
    cat ${barnes} | sed "s%inputr1='XXXX'%inputr1='${output%/}/3D_put/upa_p.nc'%g" \
        | sed "s%inputr2='XXXX'%inputr2='${output%/}/3D_put/surf_p.nc'%g"\
        | sed "s%output3d='XXXX'%output3d='${output%/}/3D_put/analysis.agrid'%g"\
        | sed "s%dir_out='XXXX'%dir_out='${output%/}/3D_put/'%g" > tmp_${seas}.pro
    
    chmod +x tmp_${seas}.pro
    barns="${workdir%/}/3D_create/Barnes_interp"
    mv tmp_${seas}.pro "${barns%/}/interpolate_model_${seas}.pro"
    old_dir=$PWD
    
    #Now run the gdl-script
    cd ${barns}
    if [  "$(which idl 2> /dev/null)" ];then
      idl_cmd='idl'
    else
      idl_cmd='gdl'
    fi
    ${idl_cmd} <<EOF
    .r sub.pro
    .r interpolate_model_${seas}.pro
    exit
EOF
    #rm ${barns%/}/interpolate_model_${seas}.pro
    cd ${old_dir}
    ${workdir%/}/3D_create/create_hume_format/create_hume_data ${output%/}/3D_put ${output%/}/3D_put
    if [ $? -ne 0 ];then
      echoerr "3D_create/create_hume_format/create_hume_data had an error, aborting"
    fi
}

clone(){
    mkdir -p "${output%/}/MWR-DATA/process_MWR/temp"
    
#   IFS=' ' read -a DATES <<< "$DATES"
    declare -a dates=${@:3}
   IFS=' ' read -a dates <<< "$dates"
    echo ${dates[1]}
    nday=$(echo ${dates[0]}|sed 's/_/ /g') #Get the first day
    let nday=$(date -u -d "${nday}" +%s) # Make it 00:00, in case it is not
    end=$(echo ${dates[1]}|sed 's/_/ /g') #Get the last date
    let end=$(date -u -d "${end}" +%s) # Make it 00:00, in case it is not
    declare -a meta_met=$(find ${input%/}/*met*.cdf)
    IFS=' ' read -a meta_met <<< "$meta_met"
    declare -a meta_mwrlos=$(find ${input%/}/*mwrlos*.cdf)
    IFS=' ' read -a meta_mwrlos <<< "$meta_mwrlos"
    while [ ${nday} -lt ${end} ] ;do
      tstring=$(date -u -d @${nday} +'%Y%m%d')
      ft_met=$(find ${input%/}/*met*$tstring*.cdf 2> /dev/null)
      ft_mwrlos=$(find ${input%/}/*mwrlos*$tstring*.cdf 2> /dev/null)
      if [ -z "$ft_met" ];then
        echo "get_micro: Creating file for met at $tstring"
        python ${workdir%/}/clone.py ${input%/} ${meta_met[0]} $tstring
      fi
      if [ -z "$ft_mwrlos" ];then
        echo "get_micro: Creating file for mwrlos at $tstring"
        python ${workdir%/}/clone.py ${input%/} ${meta_mwrlos[0]} $tstring
      fi

      nday=$(($nday+86400)) #Increase by one day (86400 sec)
    done
    }

get_micro_input(){

    mkdir -p "${output%/}/MWR-DATA/process_MWR/temp"
    
#   IFS=' ' read -a DATES <<< "$DATES"
    declare -a dates=${@:3}
   IFS=' ' read -a dates <<< "$dates"
    echo ${dates[1]}
    nday=$(echo ${dates[0]}|sed 's/_/ /g') #Get the first day
    let nday=$(date -u -d "${nday}" +%s) # Make it 00:00, in case it is not
    end=$(echo ${dates[1]}|sed 's/_/ /g') #Get the last date
    let end=$(date -u -d "${end}" +%s) # Make it 00:00, in case it is not
    declare -a meta_met=$(find ${input%/}/*met*.cdf)
    IFS=' ' read -a meta_met <<< "$meta_met"
    declare -a meta_mwrlos=$(find ${input%/}/*mwrlos*.cdf)
    IFS=' ' read -a meta_mwrlos <<< "$meta_mwrlos"
    #while [ ${nday} -lt ${end} ] ;do
    #  tstring=$(date -u -d @${nday} +'%Y%m%d')
    #  ft_met=$(find ${input%/}/*met*$tstring*.cdf 2> /dev/null)
    #  ft_mwrlos=$(find ${input%/}/*mwrlos*$tstring*.cdf 2> /dev/null)
    #  if [ -z "$ft_met" ];then
    #    echo "get_micro: Creating file for met at $tstring"
    #    python ${workdir%/}/clone.py ${input%/} ${meta_met[0]} $tstring
    #  fi
    #  if [ -z "$ft_mwrlos" ];then
    #    echo "get_micro: Creating file for mwrlos at $tstring"
    #    python ${workdir%/}/clone.py ${input%/} ${meta_mwrlos[0]} $tstring
    #  fi

    #  nday=$(($nday+86400)) #Increase by one day (86400 sec)
    #done
    python ${workdir%/}/process_MWR/date.py $seas ${dates[*]}
    mapfile -t  years_str <  ${workdir%/}/process_MWR/.years_${seas}
    mapfile -t months_str < ${workdir%/}/process_MWR/.months_${seas}
    mapfile -t first < ${workdir%/}/process_MWR/.first_${seas}
    mapfile -t last < ${workdir%/}/process_MWR/.last_${seas}

    echo ${months_str[*]}|sed "s/'//g" > ${workdir%/}/process_MWR/.months_${seas}
    echo ${years_str[*]}|sed "s/'//g" > ${workdir%/}/process_MWR/.years_${seas}

    mapfile -t  years_int <  ${workdir%/}/process_MWR/.years_${seas}
    mapfile -t  months_int <  ${workdir%/}/process_MWR/.months_${seas}

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
       sed "s%smet_vd%$smet_vd%g" > tmp_${seas}.pro
       #sed "s%readvars=['base_time','time_offset','precip_mean','temp_mean','relh_mean','lo_wind_spd_vec_avg','lo_wind_dir_vec_avg','atmos_pressure']%readvars=['base_time','time_offset','org_precip_rate_mean','temp_mean','rh_mean','wspd_vec_mean','wdir_vec_mean','atmos_pressure']%g"|\
    chmod +x tmp_${seas}.pro
       mwr_vprecip=$(echo $smet_vprecip | tr [a-z] [A-Z])
    mv tmp_${seas}.pro ${workdir%/}/process_MWR/process_${1}_a1_darwin_${seas}.pro
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
       sed "s%last=XXX%last=[${last}]%g"> tmp_${seas}.pro

    mkdir -p ${output%/}/MWR-DATA/Plot
    mkdir -p ${output%/}/MWR-DATA/ascii_out
    chmod +x tmp_${seas}.pro
    mv tmp_${seas}.pro ${workdir%/}/process_MWR/process_${2}_a1_darwin_${seas}.pro
    old_dir=$PWD
    cd ${workdir%/}/process_MWR
    if [  "$(which idl 2> /dev/null)" ];then
      idl_cmd='idl'
    else
      idl_cmd='gdl'
    fi
    ${idl_cmd} <<EOF
	   print, "working on process_${1}_a1_darwin_${seas}.pro"
    .r process_${1}_a1_darwin_${seas}.pro
    exit
EOF
  if [ $? -ne 0 ];then
    echoerr "get_micro_input had an error, aborting"
  fi

    mv ${output%/}/MWR-DATA/*.asc ${output%/}/MWR-DATA/ascii_out/

    ${idl_cmd} <<EOF
    print, "working on process_${2}_a1_darwin_${seas}.pro"
    .r process_${2}_a1_darwin_${seas}.pro
    exit
EOF
  if [ $? -ne 0 ];then
    echoerr "get_micro_input had an error, aborting"
  fi


    rm ${workdir%/}/process_MWR/process_*_a1_darwin_${seas}.pro
    units=$(ncdump -h ${output%/}/2D_put/${filename}|grep 'time:units'|cut -d = -f2|sed 's/;//'|sed 's/^ *//'|sed 's/\"//g'|sed 's/[ \t]*$//g')
    units="days since $fy-$fm-$fd 00:00:00 UTC"
    outf=${output%/}/MWR-DATA/mwrlos_6h_interp.nc
    ncatted -a units,time,o,c,"$units" $outf
  if [ $? -ne 0 ];then
    echoerr "get_micro_input had an error, aborting"
  fi

  rm -rf "${output%/}/MWR-DATA/process_MWR"

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
    dates=$(python -c "from datetime import datetime, timedelta as td;\
        d1,d2=datetime.strptime('${DATES_FIRST}','%Y%m%d'),\
        datetime.strptime('${DATES_LAST}','%Y%m%d');\
        dt=d2-d1;d=[(d1+td(days=i)).strftime('%Y%m%d') for i in range(dt.days+1)];\
        print(' '.join(d).strip('\n'))")
    python ${workdir%/}/process_rain/mask.py ${seas} #${workdir%/}/process_rain/tmp.nc ${workdir%/}/process_rain/mask.nc
    if [ $? -ne 0 ];then
          echoerr "$proc : mask.py in get_rain_input had an error, aborting"
    fi
    #Get the number of max. threads
    IFS=' ' read -a array <<< "$dates"
    base_date=$(python -c "from datetime import datetime;\
        d1=datetime.strptime('${array[0]}','%Y%m%d');\
        print(d1.strftime('%Y%m01'))")
    #let nproc=$(get_num_process)
    let nproc=1
    ary_split=$(python ${workdir%/}/split.py $nproc ${dates[*]})
    let proc=1
    #for a in ${ary_split[*]};do
    tmp_cpol=$(dirname ${output%/})/tmp_cpol
    for d in ${dates[*]};do
      #Loop through all threads and distribute the dates
      #rain_loop $base_date $proc $a #&
      ${workdir%/}/process_rain/create_dom_avg_pdf ${raininput%/} $tmp_cpol ${rainformat} ${workdir%/}/process_rain/ $seas $base_date ${d}
      if [ $? -ne 0 ];then
          echoerr "$proc : get_rain had an error, aborting"
        fi

      let proc=${proc}+1
    done
    wait
    # Create the rain ensemble time series
        ${workdir%/}/process_rain/create_timeseries \
        ${tmp_cpol%/}/domain_avg_6hr ${output%/}/radar_rain \
        ${array[0]} 0000 ${array[${#array[@]} - 1]} 2300 ${base_date} 6
    rm -rf $tmp_cpol
    rm -f ${workdir%/}process_rain/mask_${seas}.nc
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


###########################################
#              FOR DEBUGGING              #
input="0"
raininput="0"
rainformat='nc'
output="0"
filename="ecmwf.nc"
workdir=$(dirname $(readlink -f $0))
va_output="0"
#
# Read the command line.
#
while [[ $# -ge 1 ]]
do
	typeset -l option="${1}"
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
		echo >&2 "E: Unknown option: ${1}"
    echo >&2 "Usage:   ${0} [OPTIONS]"|sed "s#./##g"
    echo >&2 "Options:"
    echo >&2 "-a , --arminput  : Input dir of the atmospheric data"
    echo >&2 "-r , --raininput : Input dir of the radar data"
    echo >&2 "-o , --output    : Output dir of the varational analysis"
    echo >&2 "-v , --va_input  : Input dir of the variational analysis"
   exit 257
		;;
	esac
done
declare -a d=( $input $raininput $va_output $output )
declare -a z=( '--arminput' '--raininput' '--output' '--va_input' )
let abort=0
for o in  {0..3}; do
  if ([ -z "${d[$o]}" ] || [ ${d[$o]} == '0' ]);then
    echo >&2 "Aborting ... ${z[$o]} option not given"
    abort=1
  fi
done
if [ "$abort" -eq 1 ];then
  exit 257
fi
#Check if the directories are existing
for d in $input $raininput ;do 
  if [ ! -d "$d" ];then
    echo >&2 "Aborting ... $d does not exsist!"
    abort=1
  fi
done
if [ "$abort" -eq 1 ];then
  exit 257
fi




cd ${workdir}
##########################################
#####Get dates:
#Get the start and end date of the wet season (Radar rain availability)
split_dates=$(python ${workdir%/}/get_dates.py $raininput $input)
if [ $? -ne 0 ];then
  echoerr 'No overlapping periods found for microw. radiom. and cpol data'
fi
seas=$(echo ${output}|rev|cut -d / -f1 |rev)
old_output=${output}
old_va_output=${va_output}
overwrite=false
let n_split=0
for d in ${split_dates};do
  DATES=$(echo $d | sed 's/,/ /g')
  IFS=' ' read -a DATES <<< "$DATES"
  first=$(echo ${DATES[0]}|cut -d '_' -f1)
  last=$(echo ${DATES[1]}|cut -d '_' -f1)
  partial="$first-$last"
  
  output=${old_output%/}/$partial
  va_output=${old_va_output%/}/$partial
  if [ ! -f "${va_output}/best_est/forcing.nc" ] || [ "$overwrite" = true ];then
    echo "##############################################################"
    echo "##############################################################"
    echo "#######                                                #######"
    echo "#######          WORKING ON $partial          #######"
    echo "#######                                                #######"
    echo "##############################################################"
    echo "##############################################################"


    #Call the create_2d_input_files script
    mkdir -p ${output}
    mkdir -p ${va_output}

    ${workdir}/2D_create/create_2d_input_files $input ${output%/}/2D_put $filename ${seas} ${DATES[*]}
    if [ $? -ne 0 ];then
      echoerr "create_2d_input_files had an error, aborting"
    fi

    ###Get the 3d_data
    ${workdir}/3D_create/create_netcdf/concatenate_arm_data $input ${output%/}/3D_put ${DATES[*]}
    if [ $? -ne 0 ];then
      echoerr "concatenate_arm_data had an error, aborting"
    fi
    get_3d_input
    if [ $? -ne 0 ];then
      echoerr "get_3d_input had an error, aborting"
    fi
    #####Get the microwave input data
    #clone 'smet' 'mwrlos' ${DATES[*]}
    get_micro_input 'smet' 'mwrlos' ${DATES[*]}
    outf=${output%/}/MWR-DATA/mwrlos_6h_interp.nc
    echo $outf |python -c "import sys;from netCDF4 import Dataset as nc;import numpy as np; f=nc([i.strip('\n') for i in sys.stdin][0],'a');f.variables['be_pwv'][:]=np.ma.masked_invalid(f.variables['be_pwv'][:]);f.close()"
    echo $outf |python -c "import sys;from netCDF4 import Dataset as nc;import numpy as np; f=nc([i.strip('\n') for i in sys.stdin][0],'a');f.variables['be_lwp'][:]=np.ma.masked_invalid(f.variables['be_lwp'][:]);f.close()"


    if [ $? -ne 0 ];then
      echoerr "get_micro_input had an error, aborting"
    fi
    ####Prepare the raindata
    get_rain_input
    if [ $? -ne 0 ];then
      echoerr "get_rain_input had an error, aborting"
    fi

    echo 'Preprocessing done, running variational analysis'

    ${workdir%/}/process.sh ${output} ${va_output}
    if [ $? -ne 0 ];then
      echoerr "3D VAR had an error, aborting"
    fi
    let n_split=$n_split+1
  fi
exit
done
echo "Var analysis for season ${seas} done"
