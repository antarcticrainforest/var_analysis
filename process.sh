#!/bin/bash

echoerr() {
  echo "$@" 1>&2
  exit 257
}


#Bash-script wrapper to run the variational-analysis

#1st the the working-dir
wrkdir=$(dirname $(readlink -f $0))
wrkdir=${wrkdir%/}

#2nd check for cmd-arguments

if [ -z "$1" ] || [ -z "$2" ];then
    echoerr "Usage: $0 va_input_path va_output_path"
else
    input=${1%/}
    output=${2%/}
fi

##################################################################
if [ ! -d ${output} ];then
    mkdir -p ${output}
fi

#Set the variables
export TZ=UTC								# Always a safe thing to do.
seas=$(python2 -c "print '${input%/}'.split('/')[-1]")
#Now define the variables we need to run the run_assim script
exe_dir=${wrkdir%/} #1
ipt_dir=${input%/}
opt_dir=${output%/}/
forcing_file='forcing.nc'
iterations='3'
ecmwf_data="2D_put/ecmwf.nc"
sfcrad_data="2D_put/ecmwf.nc"		# Surface radiative fluxes for the domain. (MODEL)
toa_data="2D_put/ecmwf.nc"			# TOA fluxes and cloud data derived from satellite products. (actually MODEl)
sfcmet_data="2D_put/ecmwf.nc"		# Surface meteorology. NOT USED
flux_data="2D_put/ecmwf.nc"			# Sensible and latent heat fluxes. (NOT USED)
mwr_data="MWR-DATA/mwrlos_6h_interp.nc"
precip_data=$(find ${input%/}/radar_rain/*NOGAUGE*.nc)
precip_data=${precip_data#${input%/}/}
analysis_data="3D_put/analysis.agrid.reformat"
sonde_data="3D_put/analysis.agrid.reformat"
grid_p_data="3D_put/surf_p.agrid.reformat"		# Pressure at the surface stations. (This is all model data)
grid_u_data="3D_put/surf_u.agrid.reformat"		# U at the surface stations.
grid_v_data="3D_put/surf_v.agrid.reformat"		# V at the surface stations.
grid_t_data="3D_put/surf_T.agrid.reformat"		# Temperature at the surface stations.
grid_rh_data="3D_put/surf_rh.agrid.reformat"		# Relative humidity at the surface stations.
grid_z_data="3D_put/surf_z.agrid.reformat"		# Geopotential height at the surface stations.

###############################################################################
###############################################################################

run_assim() {
#
# Read the command line.
#
while [[ $# -gt 1 ]]
do
	typeset -u option="${1}"
	case "${option}" in
		( "-I" | "--IPT_DIR" )
		ipt_dir="${2:-${ipt_dir}}"
		shift; shift
		;;
		( "-O" | "--OPT_DIR" )
		opt_dir="${2:-${opt_dir}}"
		shift; shift
		;;
		( "-E" | "--EXE_DIR" )
		exe_dir="${2:-${exe_dir}}"
		shift; shift
		;;
		( "-F" | "--FORCING_FILE" )
		forcing_file="${2:-${forcing_file}}"
		shift; shift
		;;
		( "-N" | "--ITERATIONS" )
		iterations="${2:-${iterations}}"
		shift; shift
		;;
		#
		# The long options follow.
		#
		( "--ECMWF_DATA" )
		ecmwf_data="${2:-${ecmwf_data}}"
		shift; shift
		;;
		( "--MWR_DATA" )
		mwr_data="${2:-${mwr_data}}"
		shift; shift
		;;
		( "--PRECIP_DATA" )
		precip_data="${2:-${precip_data}}"
		shift; shift
		;;
		( "--SFCRAD_DATA" )
		sfcrad_data="${2:-${sfcrad_data}}"
		shift; shift
		;;
		( "--TOA_DATA" )
		toa_data="${2:-${toa_data}}"
		shift; shift
		;;
		( "--SFCMET_DATA" )
		sfcmet_data="${2:-${sfcmet_data}}"
		shift; shift
		;;
		( "--FLUX_DATA" )
		flux_data="${2:-${flux_data}}"
		shift; shift
		;;
		( "--ANALYSIS_DATA" )
		analysis_data="${2:-${analysis_data}}"
		shift; shift
		;;
		( "--SONDE_DATA" )
		sonde_data="${2:-${sonde_data}}"
		shift; shift
		;;
		( "--GRID_P_DATA" )
		grid_p_data="${2:-${grid_p_data}}"
		shift; shift
		;;
		( "--GRID_U_DATA" )
		grid_u_data="${2:-${grid_u_data}}"
		shift; shift
		;;
		( "--GRID_V_DATA" )
		grid_v_data="${2:-${grid_v_data}}"
		shift; shift
		;;
		( "--GRID_T_DATA" )
		grid_t_data="${2:-${grid_t_data}}"
		shift; shift
		;;
		( "--GRID_RH_DATA" )
		grid_rh_data="${2:-${grid_rh_data}}"
		shift; shift
		;;
		( "--GRID_Z_DATA" )
		grid_z_data="${2:-${grid_z_data}}"
		shift; shift
		;;
		( * )
		echoerr "E: Unknown option: ${1}"
		;;
	esac
done
#
# Check that a minimum number of iterations has been specified.
#
if [[ ${iterations} -lt 3 ]]
then
	echoerr "E: You must do at least 3 iterations of the variational analysis"
fi

#
# Check that the multitude of required directories and data files exist.
#

for dir in "${ipt_dir}" "${opt_dir}" "${exe_dir}"
do
	if [[ ! -d "${dir}" ]]
	then
		echoerr "E: No such directory: ${dir}"
	fi
done
for file in "${ecmwf_data}" "${mwr_data}" "${precip_data}" "${sfcrad_data}" "${toa_data}" "${sfcmet_data}" "${flux_data}" \
	"${analysis_data}" "${sonde_data}" "${grid_p_data}" "${grid_u_data}" "${grid_v_data}" "${grid_t_data}" "${grid_rh_data}" \
	"${grid_z_data}"
do
	if [[ ! -f "${ipt_dir%/}"/"${file}" ]]
	then
		echoerr "E: No such file: ${ipt_dir%/}/${file}"
	fi
done
#
# Remove output files from previous runs of the software.
#
rm -f "${opt_dir%/}"/output2d.nc \
	"${opt_dir%/}"/output3d.nc \
	"${opt_dir%/}"/filtered_3d_data.nc \
	"${opt_dir%/}"/outputbudget.nc \
	"${opt_dir%/}"/outputassim_state.nc \
	"${opt_dir%/}"/outputassim_budget.nc \
	"${opt_dir%/}"/"${forcing_file}" \
	"${opt_dir%/}"/"${forcing_file%.nc}.txt"
#
# Run 2D_PUT
#
 "${exe_dir%/}"/2d_put <<- END
${ipt_dir%/}/${ecmwf_data}
${ipt_dir%/}/${mwr_data}
${ipt_dir%/}/${precip_data}
${ipt_dir%/}/${sfcrad_data}
${ipt_dir%/}/${toa_data}
0
0
${opt_dir%/}/output2d.nc
END
#
# Run 3D_PUT
#
#echo ${analysis_data},${sonde_data},${grid_p_data},${grid_u_data}
#echo ${gird_v_data},${gird_t_data},${grid_rh_data},${grid_z_data}

"${exe_dir%/}"/3d_put <<- END
${ipt_dir%/}/${analysis_data}
${ipt_dir%/}/${sonde_data}
${ipt_dir%/}/${grid_p_data}
${ipt_dir%/}/${grid_u_data}
${ipt_dir%/}/${grid_v_data}
${ipt_dir%/}/${grid_t_data}
${ipt_dir%/}/${grid_rh_data}
${ipt_dir%/}/${grid_z_data}
${opt_dir%/}/filtered_3d_data.nc
${opt_dir%/}/output3d.nc
END
#
# Run BUDGET_PUT
#
"${exe_dir%/}"/budget_put <<- END
${opt_dir%/}/output2d.nc
${opt_dir%/}/output3d.nc
${opt_dir%/}/outputbudget.nc
END
#
# Run the variational analysis. 
#
echo ${opt_dir%/}/output3d.nc > "${opt_dir}"/va.ipt
echo ${opt_dir%/}/outputbudget.nc >> "${opt_dir}"/va.ipt
iter_num=3
while [[ ${iter_num} -lt ${iterations} ]]
do
	echo "Y" >> "${opt_dir%/}"/va.ipt
	iter_num=$((${iter_num}+1))
done
echo "N" >> "${opt_dir%/}"/va.ipt
echo ${opt_dir%/}/outputassim_state.nc >> "${opt_dir}"/va.ipt
echo ${opt_dir%/}/outputassim_budget.nc >> "${opt_dir}"/va.ipt

"${exe_dir%/}"/variational_analysis < "${opt_dir}"/va.ipt

rm -f "${opt_dir}"/va.ipt

#
# Now output the forcing data.
#
"${exe_dir%/}"/process_va_output <<- END
${opt_dir%/}/outputassim_state.nc
${opt_dir%/}/outputassim_budget.nc
${ipt_dir%/}/${ecmwf_data}
${ipt_dir%/}/${mwr_data}
${ipt_dir%/}/${precip_data}
0
${ipt_dir%/}/${sfcrad_data}
${ipt_dir%/}/${toa_data}
0
${ipt_dir%/}/${grid_p_data}
${opt_dir%/}/${forcing_file}
${opt_dir%/}/${forcing_file%.nc}.txt
END
}

###############################################################################
###############################################################################
ensemble_wrapper() {
    #run an va-ensemble of different rain rates
    i=-1
    for rain in $(find ${input%/}/radar_rain/*);do
       if [[ i -eq -1 ]];then
            dir="${output%/}/best_est/"
       else
            ii=$(echo $i|awk '{printf("%02d\n",$1)}')
            dir="${output%/}/p${ii}/"
        fi
        if [ ! -d $dir ];then
            mkdir -p $dir
        fi
        
        #Run the variational analysis wrapper function
        run_assim -O ${dir%/} --PRECIP_DATA ${rain#${ipt_dir%/}/}
        exit
        if [ $? -ne 0 ];then
          exit 257
        fi
        i=$(($i+1))
    done
}

#####3rd run the ensemble_wrapper function
ensemble_wrapper
