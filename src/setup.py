import os,sys,glob,datetime,platform,time,re,stat
from subprocess import Popen,PIPE
Os=platform.system()

global sleep
sleep = 0.2
if Os.lower() == 'darwin':
    Os='Osx'
    Path='/opt/local'
else:
    Os='Linux'
    Path='/usr'

def file_search(file):
    time.sleep(sleep)
    for path in os.environ['PATH'].split(':'):
        fn=os.path.join(path,file)
        if os.path.isfile(os.path.join(path,fn)):
            return True,fn
    return False,None

def checkenv(var,alt):
    try:
        return os.environ[var]
    except KeyError:
        return alt

#netcdfmod=os.popen('locate netcdf.mod 2> /dev/null').read().strip()

FC=checkenv('FC','gfortran')
CC=checkenv('CC','gcc')
FFLAGS=checkenv('FFLAGS','-ffixed-line-length-0 -std=legacy -g -O3 -fimplicit-none -fsign-zero -fbounds-check -Wpedantic -fno-automatic')
CFLAGS=checkenv('CFLAGS','-O3 -Wpedantic')
INCLUDE=checkenv('INCLUDE',os.path.join(Path,'include')).replace(':',',')
LDFLAGS=checkenv('LD_LIBRARY_PATH',os.path.join(Path,'lib')).replace(':',',')
FLIBS=checkenv('FLIBS','netcdff')
CLIBS=checkenv('CLIBS','netcdf,m')
BATCH=checkenv('BATCH',0)
PROJECT=checkenv('PROJECT',None)
MAIL=checkenv('EMAIL','')
PREFIX=os.path.dirname(os.path.dirname(os.path.abspath(__file__)))
#if len(netcdfmod):
#    INCLUDE+=',%s'%os.path.dirname(netcdfmod)
try:
    ar=sys.argv[1:]
    help=False
    for b in ar:
        a=b.replace('--','').upper()
        if b.startswith('-h') or b.startswith('--h'):
            help=True
        if b.lower().startswith('--prefix'):
            PREFIX = a.lower().split('=')[1]
        if a.startswith('FC'):
            FC=a.split('=')[-1]
        if a.startswith('FFLAGS'):
            FCFLAGS=a.split('=')[-1].replace(',',' ')
        if a.startswith('CFLAGS'):
            CFLAGS=a.split('=')[-1].replace(',',' ')
        if a.startswith('INCLUDE'):
            INCLUDE=a.split('=')[-1]
        if a.startswith('LD_LIBRARY_PATH'):
            LDFLAGS=a.split('=')[-1]
        if a.startswith('CLIBS'):
            CLIBS=a.split('=')[-1]
        if a.startswith('FLIBS'):
            FLIBS=a.split('=')[-1]
        if a.startswith('BATCH'):
            batch=a.split('=')[-1].lower()
            try :
              BATCH={'p':'pbs','s':'slurm','ll':'ll'}[batch[0]]
            except KeyError:
              sys.stderr.write('Batch option should be one of the following: pbs, slurm\n')
              sys.exit(257)
        if a.startswith('PROJECT'):
          PROJECT=a.split('=')[-1]
        if a.startswith('EMAIL'):
          MAIL=a.split('=')[-1]

except IndexError:
    print(help)
    help=True
txt="""
%s configures variational analysis to adapt to many kinds of systems.
Usage: %s [OPTION]... [VAR=VALUE]...

To assign environment variables (e.g., CC, CFLAGS...), specify them as
VAR=VALUE.  See below for descriptions of some of the useful variables.

Defaults for the options are specified in brackets

Configuration:
    -h, --help              display this help and exit

Installation directories:
    --prefix=PREFIX         install architecture-independent files in PREFIX
                            [default: %s]
    By default, 'make' will install all the files in
    %s etc.  You can specify and installation prefix by using --prefix
    for instance --prefix=$HOME

Some influential environment variables:
     FC             Fortran compiler command
                    [default gfortran]
     CC             C compiler command
                    [default gcc]
     FCFLAGS        Fortran compiler flags
                    [default -O3 -Wpedantic -fimplicit-none -fsign-zero]
     CFLAGS         C compiler flags
                    [default -O3 -Wpedantic]
     LD_LIBRAY_PATH  linker flags, e.g. -L<lib dir> if you have libraries in a
                    nonstandard directory <lib dir>
                    [default %s]
     INCLUDE        include flags e.g. -I<include dir> in a
                    the headers in a nonstandard directory <include dir>
                    [default %s]
     FLIBS          fortran libraries to pass to the linker, e.g. -l<library>
                    [default netcdff]
     CLIBS          c libraries to pass to the linker, e.g. -l<library>
                    [default netcdf,m]
     BATCH          batch system to submit jobs to a computing cluster e.g. PBS
                    [default None]
     PROJECT        project that is used to charge cpu time when BATCH is set
                    e.g. w42
                    [default None]
     EMAIL          email address of the user
                    [default None]


    Use these variables to override the choices made by %s or to help
    it to find libraries and programs with nonstandard names/locations.

    Report bugs to <martin.bergemann@met.fu-berlin.de>.

""" %(sys.argv[0],sys.argv[0],PREFIX,PREFIX,os.path.join(Path,'libs'),os.path.join(Path,'include'),sys.argv[0])
if help:
   sys.exit(txt)

altnames=dict(sed='sed',awk='awk',date='date')

LDFLAGS=LDFLAGS.split(',')


missing_package=[]

checkbin=(\
        ('gnu make','make',1),
        ('fortran compiler',FC,1),
        ('nc-config','nc-config',1),
        ('ncap','ncap',1),
        ('ncatted','ncatted',1),
        ('ncbo','ncbo',1),
        ('ncdiff','ncdiff',1),
        ('ncdump','ncdump',1),
        ('ncea','ncea',1),
        ('ncecat','ncecat',1),
        ('nces','nces',1),
        ('ncgen','ncgen',1),
        ('ncks','ncks',1),
        ('ncrcat','ncrcat',1),
        ('ncrename','ncrename',1),
        ('ncwa','ncwa',1),
        ('date','date',2),
        ('awk','awk',2),
        ('sed','sed',2),
        ('gdl or idl','gdl',2)
        )
for w,file,stats in checkbin:
    sys.stdout.flush()
    sys.stdout.write('Checking for %s ... '%w)
    sys.stdout.flush()
    status,fn=file_search(file)
    if status:
        sys.stdout.write('%s\n'%fn)
    else:
        if stats == 2:
            if file != 'gdl':
                status,fn=file_search('g%s'%file)
                altnames[file]='g%s'%file
            else:
                status,fn=file_search('idl')
            if status:
                sys.stdout.write('%s\n'%fn)
            else:
                sys.stdout.write('error, not found\n')
                missing_package.append(w)
        elif stats == 1 :
                sys.stdout.write('error, not found\n')
                missing_package.append(w)
        elif stats == 0:
                sys.stdout.write('warning, not found\n')
missing_module=[]
if BATCH:
  method=dict(pbs='qsub',slurm='sbatch')[BATCH]
  BATCH=BATCH.lower()
  proj_file=os.path.join(os.pardir,'.proj')
  if not PROJECT:
    a=raw_input('Warning option for creating batch jobs is set but no PROJECT if given, is that correct? [Y|n]: ')
    if a.lower()[0] in ( 'n', '0' , 'f' ):
      sys.exit(257)
    os.system('touch %s'%proj_file)
  else:
    f=open(proj_file,'w')
    f.write(PROJECT)
    f.close()
  if len(MAIL) == 0:
    MAIL = raw_input('No user email is given, enter now: ')
  if BATCH == 'pbs':
    batch_header=u"""#PBS -P ${pro}
# set stdout/stderr location
#PBS -o ${jobdir%/}/seas${seas}.out
#PBS -e ${jobdir%/}/seas${seas}.err
#PBS -l wd
# email options (abort,beg,end)
#PBS -m ae
#PBS -M XXX
#PBS -cwd
# set name of job
#PBS -N seas$seas
#PBS -q normal
#PBS -l walltime=00:02:00
#PBS -l mem=4GB
#PBS -l nodes=1:ppn=8
"""
  elif BATCH == 'slurm':
    batch_header =u"""#SBATCH --job-name=seas$seas
#SBATCH --time=02:00:00
#SBATCH --mem=4000
#SBATCH --mail-type=END,FAIL
#SBATCH --mail-user=XXX
#SBATCH --output=${jobdir%/}/seas${seas}.out
#SBATCH --error=${jobdir%/}/seas${seas}.err
#SBATCH --cpus-per-task=1
#SBATCH --nodes=1
#SBATCH --ntasks=8
"""
  batch_header=batch_header.replace('XXX',MAIL.lower())
  batch_job='''#!/bin/bash

echoerr() {
  echo "$@" 1>&2
  exit 257
}


pro=$(cat $(dirname $0)/.proj)
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
    echo "-s , --seas      : The name of the season [e.g. 9900]"
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
rm -rf ~/.va_jobs/THE_PBS_submit-${seas}.sh 2> /dev/null
cat << EOF >> ~/.va_jobs/pbs_submit-${seas}.sh
#!/bin/bash
# set project
THE_SCRIPT
# Now construct the job cmd
${workdir%/}/preprocess.sh -a $arminput -r $raininput -v $va_input -o $output

EOF

chmod +x ~/.va_jobs/THE_PBS_submit-${seas}.sh
echo submitting ~/.va_jobs/THE_PBS_submit-${seas}.sh via YYYY
YYYY ~/.va_jobs/THE_PBS_submit-${seas}.sh
'''
  batch_job = batch_job.replace('THE_SCRIPT',batch_header).replace('THE_PBS',BATCH)
  bash_script = os.path.join(os.pardir,'submit_%s.sh'%BATCH)
  f=open(bash_script,'w')
  f.write(batch_job.replace('YYYY',method))
  f.close()
  os.chmod(bash_script, os.stat(bash_script).st_mode | stat.S_IEXEC)

for module in ['netCDF4','datetime','numpy','glob']:
    sys.stdout.flush()
    sys.stdout.write('Checking for python module %s ... '%module)
    time.sleep(sleep)
    sys.stdout.flush()
    try:
        __import__(module)
        sys.stdout.write('ok\n')
    except ImportError:
        sys.stdout.write('module %s not found\n'%module)
        missing_module.append(module)

if len(missing_module) > 0 or  len(missing_package) > 0:
    if len(missing_package):
        sys.stderr.write("Error: The following %s packages aren't installed:\n"\
            "\t %s\n if they are installed try changing the PATH environment variable \n"%(Os,' '.join(missing_package)))
    if len(missing_module):
        sys.stderr.write("Error: The following python modules aren't installed:\n"\
            "\t %s\n if they are installed try changing the PYTHONPATH environment variable \n"%(' '.join(missing_module)))
    sys.exit(1)


version_conflict=[]
for command,min_v in (('sed',3.5),('bash',3.5),('date',6.0),('awk',3.1)):
    try:
        cmd=altnames[command]
    except KeyError:
        cmd=command
    sys.stdout.flush()
    sys.stdout.write('checking version for %s ... '%command)
    sys.stdout.flush()
    time.sleep(sleep)
    process=Popen([cmd,'--version'],stdout=PIPE)
    (output, err) = process.communicate()
    exit_code = process.wait()
    fl = float('.'.join(re.findall(r'\d+',output.split('\n')[0])[0:2]))
    if fl < min_v:
        sys.stdout.write('%2.2f < %2.2f\n'%(fl,min_v))
        version_conflict.append(command)
    else:
        sys.stdout.write('%2.2f\n'%fl)

if len(version_conflict):
    sys.stderr.write('the following packages are out of date, and need to be updated:\n'
            '\t %s\n'%(' '.join(version_conflict)))
    sys.exit(1)
for libs in (CLIBS.split(','),FLIBS.split(',')):
    for l in libs:
        status=False
        sys.stdout.write('checking status of %s... '%l)
        time.sleep(sleep)
        for path in LDFLAGS:
            if len(glob.glob(os.path.join(path,'*'+l+'*'))):
                status=True
        if status:
            sys.stdout.write('ok \n')
        else:
            sys.stdout.write('missing\n')
            sys.stdout.write('if this library is installed try changing your LD_LIBRARY_PATH variable, note that you MUST have installed the Fortran AND C netcdf libraries\n')
            sys.exit(1)

try:
    os.mkdir(os.path.join(os.path.dirname(__file__),'test'))
except OSError:
    pass

makefile_var ="""
# Makefile for variational analysis code, and various testing routines.
#
#
# created %s
#
#
FC		= %s
PREFIX		= %s
FCFLAGS		= %s
LDFLAGS		= %s
INCLUDE 	= %s
LIBS		= %s
FFLAGS		= $(FCFLAGS) $(LDFLAGS) $(INCLUDE)

SOURCE 		= portable.f90 \\
                constants.f90 \\
                physics.f90 \\
                lu.f90 \\
                settings.f90 \\
                numerics.f90 \\
                time.f90 \\
                io.f90 \\
                variational_analysis.f90 \\
                3d_put.f90 \\
                2d_put.f90 \\
                budget_put.f90 \\
                process_va_output.f90

OBJS		:= $(SOURCE:.f90=.o)

.SUFFIXES:
.SUFFIXES:	.o .f90 .mod

.f90.o:
		$(FC) -c $(FFLAGS) $*.f90

.f90.mod:
		$(FC) -c $(FFLAGS) $*.f90

all: untar source

untar :
	@echo "############## EXTRACTING TESTFILES ############################"
	tar xvjf test.tar.bz2
source:		$(OBJS)
		$(FC) $(FFLAGS) variational_analysis.o time.o lu.o constants.o settings.o portable.o physics.o numerics.o \\
			io.o -o test/variational_analysis $(LIBS)
		$(FC) $(FFLAGS) time.o lu.o constants.o portable.o settings.o 3d_put.o io.o physics.o numerics.o \\
			-o test/3d_put $(LIBS)
		$(FC) $(FFLAGS) time.o lu.o constants.o portable.o settings.o 2d_put.o io.o physics.o numerics.o \\
			-o test/2d_put $(LIBS)
		$(FC) $(FFLAGS) time.o lu.o constants.o portable.o settings.o budget_put.o io.o physics.o numerics.o \\
			-o test/budget_put $(LIBS)
		$(FC) $(FFLAGS) lu.o time.o constants.o portable.o settings.o process_va_output.o io.o physics.o numerics.o \\
			-o test/process_va_output $(LIBS)
		cd ./raerr; make
		@echo "########### MAKE TESTS #############################"
		cd test ; ./preprocess.sh all 2> ../test.out
		@echo    Test output wirtten to test.out
		@echo    Check test result for errors if desired
		@echo "########### TESTS DONE ############################"
		@echo " Now type 'make install' and 'make clean' "

clean:
		rm -rf *.o lu.mod constants.mod io.mod settings.mod portable.mod physics.mod numerics.mod time.mod core $(PREFIX)/variational_analysis $(PREFIX)/*_put $(PREFIX)/process_va_output test test.out
		cd ./raerr; make clean
install:
		mv test/process_va_output $(PREFIX)/
		mv test/budget_put $(PREFIX)/
		mv test/2d_put $(PREFIX)/
		mv test/3d_put $(PREFIX)/
		mv test/variational_analysis $(PREFIX)/
		cd ./raerr; make install
"""%(datetime.datetime.today().strftime("%e. %B %Y"),FC,PREFIX,FFLAGS,'-L'+' -L'.join(LDFLAGS),'-I'+' -I'.join(INCLUDE.split(',')),'-l'+' -l'.join(FLIBS.split(',')))
makefile_radar ="""
# Makefile for radar error code, and various testing routines.
#
# 
# created %s
#
#
CC		= %s
PREFIX		= %s
CCFLAGS		= %s
LDFLAGS		= %s
INCLUDE 	= %s
LIBS		= %s
CFLAGS		= $(CCFLAGS) $(LDFLAGS) $(INCLUDE)

SOURCE		= radar_error.c \
			  read_command.c \
			  read_error_stats.c \
			  deallocate.c \
			  read_radar_data.c \
			  calculate_pdfs.c \
			  distance.c \
			  find_percentile_error.c \
			  lncdf.c \
			  write_pdfs.c
OBJS		:= $(SOURCE:.c=.o)
EXE			= radar_error
.SUFFIXES:	.o .c
.c.o:
			$(CC) -c $(CFLAGS) $*.c
all:		$(EXE)
$(EXE):		$(OBJS)
			$(CC) $(CFLAGS) $(OBJS) $(LIBS) -o ../test/process_rain/$(EXE)
clean:
			rm -f $(OBJS) core $(EXE)
install:
			mv ../test/process_rain/$(EXE) $(PREFIX)/process_rain/
"""%(datetime.datetime.today().strftime("%e. %B %Y"),CC,PREFIX,CFLAGS,'-L'+' -L'.join(LDFLAGS),'-I'+' -I'.join(INCLUDE.split(',')),'-l'+' -l'.join(CLIBS.split(',')))







sys.stdout.flush()
sys.stdout.write("Creating Makefiles ... ")
sys.stdout.flush()
time.sleep(2)
f=open('Makefile','w')
f.write(makefile_var)
f.close()
f=open(os.path.join(os.path.dirname(__file__),'raerr','Makefile'),'w')
f.write(makefile_radar)
f.close()
sys.stdout.write("ok\n")
sys.stdout.write("Now type 'make' to compile the source code and 'make install' to install the VA programms\n")

