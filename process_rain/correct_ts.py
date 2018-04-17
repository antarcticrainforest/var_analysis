from netCDF4 import Dataset as nc
from netCDF4 import date2num
import numpy as np
import sys
from datetime import datetime
try:
    infile=sys.argv[1]
except IndexError:
    sys.exit('Error: \n Usage: %s infile.nc' %sys.argv[0])

with nc(infile,'a') as f:
  for v in ('rain_rate','rain_rate_pdf'):
    try:
      f.variables[v][:]=f.variables[v][:].filled(0.0)
    except AttributeError:
      pass
