import pandas as pd
import numpy as np
import glob, os, sys
from netCDF4 import Dataset as nc, num2date, date2num

helpstring='''Usage:
  python %s -d path_to_va -p percentile -o path_to_output
  '''%(os.path.basename(sys.argv[0]))

DIR = os.path.abspath(os.path.expanduser('~/Data'))
OUT = os.path.abspath(os.path.expanduser('~/Data'))
PERC= 'best_est'
nn = 0
try:
  for i, s in enumerate(sys.argv[1:]):
    if s.startswith('-'):
      if not s.replace('-','').replace('=','').lower()[0] in ('d','p','o'):
        sys.stderr.write(helpstring+'\n')
        sys.exit(1)
      else:
        if '=' in s:
          key,value = s.replace('-','').split('=')
          if key.lower().startswith('p'):
            try:
              PERC='p%02i'%int(value.lower())
            except ValueError:
              PERC=value.lower()
          elif key.lower().startswith('d'):
            DIR=os.path.expanduser(value)
          elif key.lower().startswith('o'):
            OUT=os.path.expanduser(value)
          else:
            sys.stderr.write(helpstring+'\n')
            sys.exit(1)
        elif s.lower().startswith('-'):
          nn = i+1
          key = s.lower().replace('-','')
          try:
            value = sys.argv[nn+1]
          except IndexError:
            sys.stderr.write('Give value after key for %s\n'%s.replace('-',''))
            sys.exit(1)
          if value.startswith('-'):
            sys.stderr.write('Expected value for key %s not a new key\n'%key)
            sys.exit(1)
          if key.lower().startswith('p'):
            try:
              PERC='p%02i'%int(value.lower())
            except ValueError:
              PERC=value.lower()
          elif key.lower().startswith('d'):
            DIR=os.path.abspath(os.path.expanduser(value))
          elif key.lower().startswith('o'):
            OUT=os.path.abspath(os.path.expanduser(value))
          else:
            sys.stderr.write(helpstring+'\n')
            sys.exit(1)
except IndexError:
  pass

if not os.path.isdir(os.path.join(OUT,PERC)):
  os.makedirs(os.path.join(OUT,PERC))

seas = ('0102', '0203', '0304', '0405', '0506', '0607', '0910', '1011', '1112', '1213', '1314' , '1415')
exclude = ('base_time', 'time_offset', 'year', 'month', 'day', 'hour', 'minute', 'lat', 'lon', 'lev', 'time', 'phis')

with pd.HDFStore(os.path.join(OUT, PERC, 'forcing.hdf5'),'w') as h5:
  with  nc(os.path.join(OUT, PERC, 'forcing.nc'),'w') as nc4:
    dims = ('time', 'lev')
    jj = 0
    for i in seas:
      individual = glob.glob(os.path.join(DIR,'va_inputs','%s/*'%i))
      individual.sort()
      for split in individual:
        output = split.replace('va_inputs', 'va_output')
        merger = os.path.join(output, 'best_est', 'forcing.nc')
        inp = os.path.join(split, '2D_put', 'ecmwf.nc')
        sys.stdout.flush()
        sys.stdout.write('Adding %s in %s .. '%(os.path.basename(split), PERC))
        sys.stdout.flush()
        if os.path.isfile(merger) and os.path.isfile(inp):
          with nc(inp) as source:
            time = pd.DatetimeIndex(num2date(source.variables['time'][1:-1],source.variables['time'].units))
          with nc(merger) as target:
            if jj == 0:
              lev = target.variables['lev'][:]
              nc4.createDimension('time', None)
              nc4.createDimension('lev', len(lev))
              nc4.createVariable('lev','f',('lev',),fill_value=-9999.)
              nc4.variables['lev'].units = 'hPa'
              nc4.variables['lev'][:] = lev
              nc4.variables['lev'].long_name = 'pressure levels'
              nc4.variables['lev'].units = 'hPa'
              nc4.variables['lev'].axis = 'Z'
              nc4.variables['lev'].missing_value = -9999.
              nc4.createVariable('time','i',('time',))
              nc4.variables['time'].units = 'seconds since 1970-01-01T00:00:00Z'
              nc4.variables['time'].axis = 'T'
              nc4.variables['time'].long_name = 'time'


            for varn in target.variables.keys():
              if jj == 0 and varn not in exclude:
                vardims = target.variables[varn].shape
                append = False
                if len(vardims) == 2:
                  nc4.createVariable(varn,'f',('time','lev'),fill_value=-9999.)
                  column = lev
                else:
                  nc4.createVariable(varn,'f',('time',),fill_value=-9999.)
                  column = ['sfc']
                for attr in ('long_name','units','missing_value'):
                  setattr(nc4.variables[varn],attr,getattr(target.variables[varn],attr))
                try:
                  data = target.variables[varn][:].filled(np.nan)
                except AttributeError:
                  data = target.variables[varn][:]
                df = pd.DataFrame(data,columns=column, index=time)
                h5.put(varn,df,append=False)
              elif varn not in exclude:
                try:
                  data = target.variables[varn][:].filled(np.nan)
                except AttributeError:
                  data = target.variables[varn][:]
                if len(data.shape) == 2:
                  column = lev
                else:
                  column = ['sfc']
                data = pd.DataFrame(data,columns=column,index=time)
                df = h5['/'+varn]

                h5.put(varn,pd.concat((df,data),axis=0))

        sys.stdout.write('ok\n')
        jj += 1
with pd.HDFStore(os.path.join(OUT, PERC, 'forcing.hdf5'),'a') as h5:
  with  nc(os.path.join(OUT, PERC, 'forcing.nc'),'a') as nc4:
    T = pd.date_range(h5['/q'].index[0], h5['/q'].index[-1],freq='10 min')
    for varn in nc4.variables.keys():
      if varn not in ('time','lev'):
        if len(nc4.variables[varn].shape) == 2:
          data = np.zeros([len(T),len(lev)]) * np.nan
          columns = lev
        else:
          data = np.zeros([len(T)]) * np.nan
          columns = ['sfc']

          df = pd.DataFrame(data,columns=columns,index=T)
          dd = h5['/'+varn]
          df.loc[dd.index] = dd
          nc4.variables[varn][:] = np.ma.masked_invalid(df.values)
      elif varn == 'time':
          nc4.variables[varn][:] = date2num(T.to_pydatetime(),nc4.variables[varn].units)
