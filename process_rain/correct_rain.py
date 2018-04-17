from netCDF4 import Dataset as nc
from netCDF4 import date2num
import numpy as np
import sys
from datetime import datetime
try:
    infile=sys.argv[1]
    avg=sys.argv[2]
except IndexError:
    sys.exit('Error: \n Usage: %s infile.nc average' %sys.argv[0])

f=nc(infile,'a')
try:
    if avg == 'full':
        f.createVariable('i','f',('i',))
        f.createVariable('j','f',('j',))
except RuntimeError:
    pass

try:
    f.createDimension('time',None)
except RuntimeError:
    pass
try:
    f.createVariable('time','f',('time',))
except RuntimeError:
    pass
time = "_".join(infile.replace("domavg","").split('_')[-3:-1])
date=datetime.strptime(time,"%Y%m%d_%H%M")
num=date2num(date,'minutes since 1970-01-01 00:00:00')

if avg == 'full':
    f.variables['i'].units='degrees_norht'
    f.variables['i'].long_name='latitude'
    f.variables['i'][:]=f.variables['lat'][:,0]

    f.variables['j'].units='degrees_east'
    f.variables['j'].long_name='longitude'
    f.variables['j'][:]=f.variables['lon'][0]

f.variables['time'][:]=np.array([num])
f.variables['time'].units='minutes since 1970-01-01 00:00:00'
f.variables['time'].long_name='time of radar scan'
f.variables['time'].axis='T'

f.close()
