import numpy as np, os, sys, glob, shutil
from netCDF4 import Dataset as nc, num2date, date2num
from datetime import datetime, timedelta













def get_periods(start,end):

  out =[]
  dt = timedelta(hours=6)
  for i in xrange(len(start)):
    out.append((start[i],end[i],True))
    if not i == len(start)-1 :
      missing_s = end[i]+dt
      missing_e = start[i+1]-dt
      out.append((missing_s,missing_e,False))

  return out

def correct_time(f):
  try:
    oldtime=num2date(f.variables['time'][:],f.variables['time'].units)
    newtime=date2num(oldtime,'seconds since 1970-01-01 00:00:00')
  except IndexError:
    newtime=None
  try:
    oldoffset=num2date(f.variables['time_offset'][:],f.variables['time_offset'].units)
    newoffset=date2num(oldoffset,'seconds since 1970-01-01 00:00:00')
  except IndexError:
    newoffset=None

  return newtime,newoffset


def concanate(source,ncf,exclude):
  
  with nc(source) as f:
    time,offset=correct_time(f)
    idx=len(ncf.variables['time'][:])+1
    ncf.variables['time'][idx:]=time
    if type(offset) != type(None):
      ncf.variables['time_offset'][idx:]=offset
    for v in ncf.variables.keys():
      if v not in exclude:
        ncf.variables[v][idx:]=f.variables[v][:]

def fill(f,tstep,end,exclude):
  dt = timedelta(hours=6)
  dates = []
  y,mon,day,Hour,Min = []
  while tstep <= end:
    dates.append(tstep)
    y.append(tstep.year)
    mon.append(tstep.month)
    day.append(tstep.day)
    Min.append(tstep.minute)
    Hour.append(tstep.hour)
    tstep += dt
  idx=len(f.variables['time'][:])+1
  times=date2num(dates,f.variables['time'].units)
  f.variables['time'][idx:]=times
  if 'time_offset' in f.variables.keys():
    offset=date2num(dates,f.variables['time_offset'].units)
    f.variables['time_offset'][idx:]=offset
  if 'year' in f.variables.keys():
    f.variables['year'][idx:]=np.array(y)
    f.variables['month'][idx:]=np.array(mon)
    f.variables['day'][idx:]=np.array(day)
    f.variables['hour'][idx:]=np.array(Hour)
    f.variables['minute'][idx:]=np.array(Min)

  for v in f.variables.keys():
    if v not in exclude:
      try:
        f.variables[v][idx:] = f.variables[v].missing_value
      except AttributeError:
        f.variables[v][idx:] = -9999.0
        f.variables[v].missing_value= -9999.0






def copy(folder,dates):
  tstring='%s-%s'%(dates[0].strftime('%Y%m%d'),dates[1].strftime('%Y%m%d'))
  sfolder=os.path.join(folder,tstring)
  tfolder=os.path.join(folder,'merge')
  os.system('cp -r %s/* %s'%(sfolder,tfolder))

def merge(folder,dates):
  exclude=('base_time','time','time_offset','lat','lon','phis','lev','string','stations','levels','variables','stru','strs','vbudget_column','vbudget_layer','weight')
  for p in xrange(100):
    if p == 0:
      t = 'best_est'
    else:
      t = 'p%02i'%p

    tfolder = os.path.join(folder,'merge',t,'*.nc')
    for tfile in glob.glob(tfolder,tfolder):
      ncfile = os.path.basename(tfile)
      sys.stdout.write('Merging %si\n'%ncfile)
      with nc(tfile,'a') as h5:
        time,offset=correct_time(h5)
        h5.variables['time'][:]=time
        h5.variables['time'].units='seconds since 1970-01-01 00:00:00'
        if type(offset) != type(None):
          h5.variables['time_offset'][:]=offset
          h5.variables['time_offset'].units='seconds since 1970-01-01 00:00:00'

        for start,end,present in dates:
          if present:
            tstring='%s-%s'%(start.strftime('%Y%m%d'),end.strftime('%Y%m%d'))
            sfile = os.path.join(folder,tstring,ncfile)
            concanate(sfile,h5,exclude)
          else:
            fill(h5,start,end,exclude)
    break
def main(folder,dates):
  starts=[]
  ends=[]
  for d in dates:
      s,e=d.split(',')[0:2]
      starts.append(datetime.strptime(s,'%Y%m%d_%H%M'))
      ends.append(datetime.strptime(e,'%Y%m%d_%H%M'))
  periods = get_periods(starts,ends)
  #copy(folder,periods[0])
  merge(folder,periods[1:])



if __name__== '__main__':

  try:
    folder=sys.argv[1]
    dates=sys.argv[2:]
  except IndexError:
    sys.stderr.write('Usage: %s va_output dates'%sys.argv[0])
    sys.exit(257)

  main(folder,dates)
