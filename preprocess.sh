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


dates () {
    #What is the working folder
    DIRS=${1%/}/
    HEAD='*ecmwf*surf*'
    
    #Get all dates
    FILES=($(find ${DIRS}${HEAD}.* 2> /dev/null))
    file=$(basename ${FILES[1]})
    head1=$(echo $file|cut -d . -f1)
    head2=$(echo $file|cut -d . -f2)
    HEAD=${head1}.${head2}


    local DATES=($(echo ${FILES[*]}|python2 -c "import sys;from datetime import datetime;\
        files=sys.stdin.read().strip('\n').split(' ');\
        dates=[\
        datetime.strptime(i,\"${DIRS}\"+'${HEAD}.%Y%m%d.000000.cdf')\
        .strftime('%Y%m%d') for i in files\
        ];\
        print ' '.join(dates)"))
   
    if [ "${rainformat}" == 'ascii' ];then
        end='ascii'
    else
        end='nc'
    fi
    if [  "$(command -v gdate 2> /dev/null)" ];then
        cmd='gdate'
    else
        cmd='date'
    fi


    bool=false
    
    for (( d=0;d<${#DATES[*]};d++ )) {
        local RDATES=$(ls ${raininput%/}/*${DATES[$d]}*.${end} 2> /dev/null)

        if [ -z "${RDATES[*]}" ] && [ "$bool" = false ];then
            DATES=(${DATES[@]/${DATES[${d}]}})
        else
            bool=true
        fi


    }
    if [ -z "$(ls ${raininput%/}/*${DATES[0]}*.${end} 2> /dev/null)" ];then
            DATES=(${DATES[@]/${DATES[0]}})
    fi



    rain=($(find ${raininput%/}/*${array[$n]}* 2>/dev/null))
    while [ -z "$files" ];do
        n=$n+1
        files=($(find ${raininput%/}/*${array[$n]}* 2>/dev/null))
    done
    if [ ${#files[*]} -le 144 ];then
        n=$n+1
    fi


    local  DATES_LAST="$(${cmd} -d "${DATES[@]:(-1)} 1800 + 1 month - 1 day" "+%Y%m%d")"
    echo ${DATES[*]}

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
    cat ${barnes} | sed "s%inputr1='XXXX'%inputr1='${output%}/3D_put/upa_p.nc'%g" \
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
   init_time="julday(${DATES[0]:4:2,2},1,${DATES[0]:0:4},0,0,0)-0.5"
   if [ ${#DATES[*]} -eq 1 ];then
        years_str="['${DATES[0]:0:4}']"
        years_int="[${DATES[0]:0:4}]"
        mon_str="['${DATES[0]:4:2}']"
   elif [ ${#DATES[*]} -eq 2 ];then
        years_str="['${DATES[0]:0:4}','${DATES[1]:0:4}']"
        years_int="[${DATES[0]:0:4},${DATES[1]:0:4}]"
        mon_str="['${DATES[0]:4:2}','${DATES[1]:4:2}']"
  else
       years_str="['${DATES[0]:0:4}'"
       years_int="[${DATES[0]:0:4}"
       mon_str="['${DATES[0]:4:2}'"
       for i in ${DATES[*]:1};do
            years_str="${years_str},'${i:0:4}'"
            years_int="${years_int},${i:0:4}"
            mon_str="${mon_str},'${i:4:2}'"
        done
        years_str="${years_str}]"
        years_int="${years_int}]"
        mon_str="${mon_str}]"
   fi
    if [ ! -d ${output%/}/MWR-DATA ];then
        mkdir -p ${output%}/MWR-DATA
    fi
    
    cat ${workdir%/}/process_MWR/.process_${1}_a1_darwin.pro| \
       sed "s%@WRKDIR/%@${workdir%/}/%g" |\
       sed "s%path_prefix='WRKDIR'%path_prefix='${workdir%/}/'%g" |\
       sed "s%data_path='INPUT'%data_path='${input%/}/'%g"|\
       sed "s%file_id='ID'%file_id='${1}'%g"|\
       sed "s%out_dir='OUTPUT'%out_dir='${output%/}/MWR-DATA/'%g"|\
       sed "s%years_int=XXX%years_int=${years_int}%g" |\
       sed "s%years_str=XXX%years_str=${years_str}%g" |\
       sed "s%months=XXX%months=${mon_str}%g" |\
       sed "s%ini_time=XXX%ini_time=${init_time}%g"> tmp.pro
    chmod +x tmp.pro
    seas=$(echo ${output}|rev|cut -d / -f2 |rev)
    mv tmp.pro ${workdir%/}/process_MWR/process_${1}_a1_darwin.pro
    cat ${workdir%/}/process_MWR/.process_${2}_a1_darwin.pro| \
       sed "s%@WRKDIR/%@${workdir%/}/%g" |\
       sed "s%path_prefix='WRKDIR'%path_prefix='${workdir%/}/'%g" |\
       sed "s%data_path='INPUT'%data_path='${input%/}/'%g"|\
       sed "s%file_id='ID'%file_id='${2}'%g"|\
       sed "s%out_dir='OUTPUT'%out_dir='${output%/}/MWR-DATA/'%g"|\
       sed "s%years_int=XXX%years_int=${years_int}%g" |\
       sed "s%years_str=XXX%years_str=${years_str}%g" |\
       sed "s%months=XXX%months=${mon_str}%g" |\
       sed "s%ini_time=XXX%ini_time=${init_time}%g"|\
       sed "s%hourly_path='INPUT'%hourly_path='${output%/}/MWR-DATA/'%g"|\
       sed "s%ascii_path='OUTPUT'%ascii_path='${output%/}/MWR-DATA/ascii_out/'%g"|\
       sed "s%OUTPUT=XXX%OUTPUT='${output%/}/MWR-DATA/'%g"|\
       sed "s%seas=XXX%seas='${seas}'%g"|\
       sed "s%XXbase_timeXX%${DATES[0]}%g"> tmp.pro

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
    
    let ntimes=$(expr $(ncdump -h ${output%/}/MWR-DATA/mwrlos_6h_${seas}_C3_interp3.nc|\
        grep 'time ='|cut -d \( -f2 |awk '{print $1}') - 1)
    let ntimes2=$(expr $(ncdump -h ${output%/}/2D_put/${filename}|\
        grep 'time ='|cut -d \( -f2 |awk '{print $1}') - 1)
    ntimes=$(($ntimes<$ntimes2?$ntimes:$ntimes2))
    
    units=$(ncdump -h ${output%/}/2D_put/${filename}|grep 'time:units'|cut -d = -f2|sed 's/;//'|sed 's/^ *//'|sed 's/\"//g'|sed 's/[ \t]*$//g')
    if [ -f "${output%/}/MWR-DATA/mwrlos_6h_${seas}_interp3.nc" ];then
        rm ${output}mwrlos_6h_${seas}_interp3.nc
    fi
    
    ncks -O -d time,0,${ntimes} \
        ${output%/}/MWR-DATA/mwrlos_6h_${seas}_C3_interp3.nc \
        ${output%/}/MWR-DATA/mwrlos_6h_${seas}_interp.nc

    ncatted -a units,time,o,c,"$units" ${output%/}/MWR-DATA/mwrlos_6h_${seas}_interp.nc

    #rm  ${output%/}/MWR-DATA/mwrlos_6h_${seas}_C3_interp3.nc
    mv ${output%/}/MWR-DATA/*.asc ${output%/}/MWR-DATA/ascii_out/



}

get_rain_input(){
    
    cmd=$(which gdate 2> /dev/null)
    if [ -z "$cmd" ];then
        cmd=$(which date)
    fi

    #First merge all 10 min files to monthly files together
    if [ ${#DATES[*]} -eq 1 ];then
        DATES_LAST=$(python2 -c "from datetime import datetime,timedelta as td;\
            from calendar import monthrange;\
            time=datetime.strptime('${DATES[0]}','%Y%m%d');\
            days=monthrange(time.year,time.month)[1];\
            print (time+td(days=days)).strftime('%Y%m%d') ")
    else
        #DATES_LAST="$(${cmd} -d "${DATES[@]:(-1)} 1800 + 1 month - 1 day" "+%Y%m%d")"
        DATES_LAST=${DATES[@]:(-1)}
    fi
    dates=$(python2 -c "from datetime import datetime, timedelta as td;\
        d1,d2=datetime.strptime('${DATES[0]}','%Y%m%d'),\
        datetime.strptime('${DATES_LAST}','%Y%m%d');\
        dt=d2-d1;d=[(d1+td(days=i)).strftime('%Y%m%d') for i in xrange(dt.days+1)];\
        print ' '.join(d).strip('\n')")
    
    python2 ${workdir%/}/process_rain/mask.py #${workdir%/}/process_rain/tmp.nc ${workdir%/}/process_rain/mask.nc
    #Get the number of max. threads
    IFS=' ' read -a array <<< "$dates"
    base_date=$(python2 -c "from datetime import datetime;\
        d1=datetime.strptime('${array[0]}','%Y%m%d');\
        print d1.strftime('%Y%m01')")
    let nproc=$(get_num_process)
    let nproc=1
    #Loop through all dates and distribute the parallel threads
    for d in ${dates[*]};do
        #Do the averaging
        ${workdir%/}/process_rain/create_dom_avg_pdf ${raininput%/} \
            ${rainformat} ${workdir%/}/process_rain/ $base_date ${d} &
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
    #exit
    done
    wait
    # Create the rain ensemble time series
    let n=0
    files=($(find ${raininput%/}/new/*${array[$n]}* 2>/dev/null))
    while [ -z "$files" ];do
        files=($(find ${raininput%/}/new/*${array[$n]}* 2>/dev/null))
        n=$n+1
    done
    if [ ${#files[*]} -le 144 ];then
        n=$n+1
    fi
    ${workdir%/}/process_rain/create_timeseries \
        ${raininput%/}/new/domain_avg_6hr ${output%/}/radar_rain \
        ${array[0]} 0000 ${array[${#array[@]} - 2]} 1200 ${base_date} 6
    #> out.err

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
prefix="/home/wilfred/Work/"
input="${prefix%/}/Input/ARM/0405/"
raininput="${prefix%/}/CPOL/0405/"
rainformat='ascii'
output="${prefix%/}/va_inputs/0405/"
filename="ecmwf.nc"
workdir="${prefix%/}/run"
va_output="${prefix%/}/va_output/0405"
##########################################

#####Get dates:
#DATES=($(dates ${input%/}/))
DATES=20050301


#Call the create_2d_input_files script
if [ ! -d "$output" ];then
    mkdir -p ${output}
fi
#${workdir}/2D_create/create_2d_input_files $input ${output%/}/2D_put $filename ${DATES[*]}
#echo "#############################"
#####Get the 3d_data
#${workdir}/3D_create/create_netcdf/concatenate_arm_data $input ${output%/}/3D_put ${DATES[*]}
#exit
#get_3d_input
#####Get the microwave input data
get_micro_input 'smet' 'mwrlos'
####Prepare the raindata
#get_rain_input
exit

echo -e "Pre-processing done now. Do you want to run the following command:\n \n \
    \t ${workdir%/}/process.sh ${output} ${va_outut} [Y/n]\n"
read question

if [ -z "${question}" ] || [ "${question}" == 'y' ] || [ "${question}" == 'Y' ] || [ "${question}" == 'yes' ] || [ "${question}" == 'Yes' ];then
    ${workdir%/}/process.sh ${output} ${va_output}
fi

