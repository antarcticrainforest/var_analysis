import numpy as np
from netCDF4 import Dataset as nc , date2num
from datetime import datetime
import os,sys

def get_timestamp(fname):

  for s in fname.split('.'):
    try:
      return datetime.strptime(s,'%Y%m%d').strftime('%Y%m%d')
    except :
      pass


def main(folder,meta,date):
  r = get_timestamp(os.path.basename(meta))
  newf = meta.replace(r,date.strftime('%Y%m%d'))
  os.system('cp %s %s '%(meta,newf))
  dimvar=('base_time','time','qc_time','qc_logger_time','time_offset')
  with nc(newf,'a') as f:
    for v in f.variables:
      if v not in dimvar and not v.startswith('lat') and not v.startswith('lon'):
        try:
          missing_val=f.variables[v].missing_value
          f.variables[v][:]=np.ones_like(f.variables[v][:])*missing_val
        except AttributeError:
          pass
    tstring='%s 00:00:00 0:00' %(date.strftime('%Y-%m-%d'))
    try:
      f.variables['base_time'].string=tstring
      f.variables['base_time'][:]=np.array(date2num(date,f.variables['base_time'].units))
    except KeyError:
      pass
    for u in ('time_offset','time'):
      try:
        unit=f.variables[u].units
        unit=unit.split(' ')[0]
        f.variables[u].units=unit+' since %s'%tstring
      except KeyError:
        pass


#  os.remove(newf)

if __name__ == '__main__':

  try:
    folder,meta,date = sys.argv[1:4]
  except ValueError:
    sys.stderr.write('Usage: %s inputdir presentfile date\n'%sys.argv[0])
    sys.exit(257)

  main(folder,meta,datetime.strptime(date,'%Y%m%d'))
