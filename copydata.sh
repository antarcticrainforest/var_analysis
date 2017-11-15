#!/bin/bash

echoerr() {
  echo "$@" 1>&2
  exit 257
}

while [[ $# -ge 1 ]]
do
	typeset -l option="${1}"
	case "${option}" in
		( "-i" | "--input" )
		input="${2:-${input}}"
		shift; shift
		;;
		( "-o" | "--output" )
		output="${2:-${output}}"
		shift; shift
		;;
		( "-s" | "--start" )
		start="${2:-${start}}"
		shift; shift
		;;
		( "-e" | "--end" )
		end="${2:-${end}}"
		shift; shift
		;;
		( * )
		echo "E: Unknown option: ${1}"
    echo "Usage:   ${0} [OPTIONS]"|sed "s#./##g"
    echo "Options:"
    echo "-o , --input     : Folder where the data was downloaded to"
    echo "-i , --output    : The parent folder containing the season folder"
    echo "-s , --start     : First data month that is in input (format YYYYMM)"
    echo "-e , --end       : Last data month that is in input (format YYYMM)"
		exit 2
		;;
	esac
done
if [ -z "$input" ];then
  echoerr "Error --input not given"
fi

if [ -z "$output" ];then
  echoerr "Error --output not given"
fi

if [ -z "$start" ];then
  echoerr "Error --start not given"
fi

if [ -z "$end" ];then
  echoerr "Error --end not given"
fi

declare -a wtseas=( [5]=0 [6]=0 [7]=0 [8]=0 [9]=0 [10]=1 [11]=1 [12]=1 [1]=1 [2]=1 [3]=1 [4]=1 )
tstep=$(date -u -d ${start}01 +%s)
last=$(date -u -d ${end}01 +%s)

while [ $tstep  -le $last ];do
  mm=$(date -u -d @${tstep} +'%m')
  let mm2=$(echo $mm|awk '{printf("%i\n",$1)}')
  yyyy=$(date -u -d @${tstep} +'%Y')
  if [ ${wtseas[$mm2]} -eq 1 ];then
    echo "Copy data for $(date -u -d ${yyyy}${mm}01 +"%m/%Y")"
    if [ $mm -ge 10 ];then
      yy1=$(echo $yyyy |rev | cut -c-2|rev)
      let yyyy1=${yyyy}+1
      yy2=$(echo $yyyy1 |rev | cut -c-2|rev)
    else
      yy2=$(echo $yyyy |rev | cut -c-2|rev)
      let yyyy1=${yyyy}-1
      yy1=$(echo $yyyy1 |rev | cut -c-2|rev)
    fi
    seas=${yy1}${yy2}

    tstring=${yyyy}${mm}
    folder=${output%}/${seas}
    mkdir -p $folder
    cp ${input%/}/*${tstring}*.* ${folder%/}/
  fi
  let mon=$mm2+1
  if [ $mon -eq 13 ];then
    let mon=1
    let yyyy=${yyyy}+1

  fi
  if [ $mon -lt 10 ];then
    mm=0$mon
  else
    mm=$mon
  fi
  tstep=$(date -u -d ${yyyy}${mm}01 +%s)
done
