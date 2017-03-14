3D Variational Analysis of ECMWF Analysis Model Data
+++++++++++++++++++++++++++++++++++++++++++++++++++++

This repository contains the source-code of a 3D Variational Analysis (3D Var)

For more details, see the scientific publications:
`Developing long-term single-column model/cloud system - resolving model forcing  data using numerical weather prediction products constrained by surface and top of the atmosphere observations <http://doi.org/10.1029/2003jd004045>`_

`Observed Large-Scale Structures and Diabatic Heating and Drying Profiles
during TWP-ICE <http://doi.org/10.1175/2009jcli3071.1>`_

This repository includes the following directories:

 * 2D_create    - directory with scripts to create the 2D input fields for the 3D Var
 * 3D_create    - and the scripts for the 3D input field
 * process_MWR  - scripts to process the microwave based observations
 * process_rain - scripts to process the radar based rainfall estimates
 * src          - the source code for the variational analysis

Prerequisites
=============
You will need:

 * `Python 2.7 Release <http://www.python.org/>`_
 * `Gnu Data Language <http://gnudatalanguage.sourceforge.net>`_
 * `netCDF Operator (nco) <http://nco.sourceforge.net>`_
 *  A working C compiler
 *  A working Fortran compiler with netcdf (netcdff) support
 

Additional Python Packages and Libraries
----------------------------------------
Additionally the following python packages must be installed on the system
to run the pattern recognition:

 * `Numpy <http://www.numpy.org/>`_
 * `Netcdf4 Library with fortran support <http://www.unidata.ucar.edu/software/netcdf/>`_
 * `Netcdf4-Python <http://netcdf4-python.googlecode.com>`_

All packages are open source and should be available via the package manager of
your OS.

To install the packages for Ubuntu 12.04 and later, and Debian wheezy and later::

   apt-get install python-numpy python-netcdf gnudatalanguage netcdf-fortran

On Arch 2012.07.15 and later::
  
  pacman -S python2-numpy netcdf-fortran
  python2-netcdf4 gnudatalanguage (via AUR)

On Gentoo Base System release 2.X and later::
  emerge numpy (with python2_7 use flag) netcdf netcdf4-python netcdf-fortran
On Mac OS X 10.6 and later (via port reposetory system)::
   
   port install py27-numpy py27-netcdf4 gnudatalanguage netcdf-fortran


   
Data and format
---------------
You might need to `create<https://www.archive.arm.gov/armuserreg/#/new>`_ an 
account at the Atmospheric Radiation Measurement (ARM) Climate Research Programm.
Once you have retreived an account you will need to download the following data:

Suppose you want to run the analysis for Feb 2014:
Got to the `arm data catalogue<http://www.archive.arm.gov/discovery/#v/results/s/>`_ and
search for :
 
* Surface data from ecmwf model (e.g. twpecmwfsurfX1 for data over Darwin)
* Supplement data from ecmwf model (e.g. twpecmwfsuppX1 for data over Darwin)
* Upper atmosphere data from ecmwf model (e.g. twpecmwfupaX1 for data over Darwin)
* Microwave Water Radiometer measurements (e.g. twpmwrlosC3.b1 for data from Darwin)
* Surface measurements (e.g. twpmetC3.b1 for data from Darwin)

If you want to retreive data from Darwin than just copy and paste the acronyms.
If not you can choose any facility that is offered on the ARM site. You only have
to make sure to download data supplemental model data from ecmwf (*ecmwfsupp*),
surface data (*surf*), upper atmosphere data (*upp*) from ecmwf, microwave
radiometer (*mwrlos*) and surface met measurements (*met*). The prefix twp stands
for tropical western pacific and the suffix X1/C3.b1 for model data/observations
over Darwin. Replace the prefixes and suffixes by the desired region and station.
The acronyms can be found on the search page under the sites and facility bars.

Once you downloaded the data make sure all files are located in ``one`` folder.

You also have to download rainfall radar data from your preferred side. Make
sure that the files are in 10 minutes resolution with a singel file for each
time step. The data can either be in ascii or netcdf format. Once again make
sure that all files are stored in the same folder.


Building
========
The ``setup.py`` script in the ``src`` folder helps you creating a Makefile 
for building the 3D Var program. Some important environment variables are:
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

Use these variables to override the choices made by setup.py or to help
it to find libraries and programs with nonstandard names/locations.

To create the Makefile simply type::
  python setup.py build

Comiple the source code with::
  make && make install


Testing
=======
Some test data with comes with the code. The data is stored in .test in 
this directory. The output of the test is written to the file ``test.out`` in 
the ``src`` directory. It is recommended to look for any suspicious errors and
warnings in the file.

Usage
=====
Once everything is downloaded and working ok run the ``preprocess.sh`` script.
The script takes the following command line arguments:

* -a , --arminput  : Input dir of the atmospheric data
* -r , --raininput : Input dir of the radar data
* -o , --output    : Output dir of the varational analysis
* -v , --va_input  : Input dir of the variational analysis

Contributing
============
We welcome all types of contributions, from blueprint designs to
documentation to testing to deployment scripts.


Bugs
====
Bugs should be filed to **martin.bergemann@monash.edu**
