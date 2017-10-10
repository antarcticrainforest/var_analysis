#!/bin/bash

echoerr() {
  echo "$@" 1>&2
  exit 257
}


pro='w42'
seas='0'
id='0'
while [[ $# -ge 1 ]]
do
	typeset -l option="${1}"
	case "${option}" in
		( "-d" | "--dir" )
		dir="${2:-${dir}}"
		shift; shift
		;;
		( "-s" | "--seas" )
		seas="${2:-${seas}}"
		shift; shift
		;;
    ( "-p" | "--proj" )
		seas="${2:-${seas}}"
		shift; shift
		;;

		( * )
		echo "E: Unknown option: ${1}"
    echo "Usage:   ${0} [OPTIONS]"|sed "s#./##g"
    echo "Options:"
    echo "-d , --dir       : Parent directory"
    echo "-p , --projcet   : The project id that is used for charing cpu time"
    echo "-s , --seas      : The name of the season [e.j 9900]"
		exit 2
		;;
	esac
done
declare -a d=( $dir $seas )
declare -a z=( '--dir' '--job_id' )
for o in  {0..1}; do
  if ([ -z "${d[$o]}" ] || [ ${d[$o]} == '0' ]);then
    echoerr "Aborting ... ${z[$o]} option not given"
  fi
done
# Construnct the directories
arminput=${dir%/}/ARM/${seas%/}
raininput=${dir%/}/CPOL/${seas%/}
va_input=${dir%/}/var_ana/va_inputs/${seas%/}
output=${dir%/}/var_ana/var_output/${seas%/}

workdir=$(dirname $(readlink -f $0))
jobdir=${workdir%/}/Jobs/
mkdir -p ~/.va_jobs
cat << EOF >> ~/.va_jobs/pbs_submit-${seas}.sh

# set project
#PBS -P $pro
# set stdout/stderr location
#PBS -o ${jobdir%/}/seas${seas}.out
#PBS -e ${jobdir%/}/seas${seas}.err
#PBS -l wd
# email options (abort,beg,end)
#PBS -m aes
#PBS -M martin.bergemann@monash.edu
#PBS -cwd
# set name of job
#PBS -N seas$seas
#PBS -q normal
#PBS -l walltime=00:02:00
#PBS -l mem=4GB
#PBS -l nodes=1:ppn=8

# Now construct the job cmd
${workdir%/}/preprocess.sh -a $arminput -r $raininput -v $va_input -o $output

EOF

chmod +x ~/.va_jobs/pbs_submit-${seas}.sh
echo submitting ~/.va_jobs/pbs_submit-${seas}.sh via qsub
echo qsub ~/.va_jobs/pbs_submit-${seas}.sh

