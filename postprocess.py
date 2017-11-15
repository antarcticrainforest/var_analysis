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


def concanate(source,ncf,exclude,start):
  ''' Function to add existing data to the target netcdffile '''
  try:
    with nc(source) as f:
      idx=len(ncf.variables['time'][:])
      for v in ncf.variables.keys():
        if v not in exclude:
          ncf.variables[v][idx:]=f.variables[v][:]
          
  except RuntimeError, OSError:
    pass
def fill(f,tstep,end,exclude):
  ''' Function to fill period with no data with missing values in the target 
      netcdf file '''
  dt = timedelta(hours=6)
  dates = []
  while tstep <= end:
    dates.append(tstep)
    tstep += dt
  idx=len(f.variables['time'][:])
  f.variables['time'][idx:]=date2num(dates,'seconds since 1970-01-01 00:00:00')
  for v in f.variables.keys():
    if v not in exclude:
      try:
        f.variables[v][idx:] = f.variables[v].missing_value
      except AttributeError:
        f.variables[v][idx:] = -9999.0
        f.variables[v].missing_value= -9999.0


def fix_time(f,start,end,exclude):
  ''' Function that fixes up the time variables and missing values '''
  dt=timedelta(hours=6)
  nn=0
  T = ('time','time_offset','year','month','day','hour','minute')
  while start < end:
    for v in T:
      if v in f.variables.keys():
        ts=date2num(start,'seconds since 1970-01-01 00:00:00')
        if v in ('year','month','day','hour','minute'):
            f.variables[v][nn]=[getattr(start,v)]
        else:
            f.variables[v][nn]=[ts]
    start += dt
    nn+= 1

  for var in f.variables.keys():
     if var not in exclude and var not in T:
       ii = np.where(f.variables[var][:] > 1e30)
       if len(ii[0] > 0):
         try:
            f.variables[var][ii] = f.variables[var].missing_value
         except AttributeError:
            f.variables[var][ii] = -9999.
            f.variables[var].missing_value=-9999.

def copy(folder,dates):
  ''' Copy the output of the first data preiod and make them the target netcdffile
      where stuff gets added '''
  tstring='%s-%s'%(dates[0].strftime('%Y%m%d'),dates[1].strftime('%Y%m%d'))
  sfolder=os.path.join(folder,tstring)
  tfolder=os.path.join(folder,'merge')
  os.system('cp -r %s/* %s'%(sfolder,tfolder))


def merge(folder,dates):
  #Which variables should not be merged because they don't change or will be fixed later?
  exclude=('base_time','time','time_offset','lat','lon','phis','lev','string','stations','levels','variables','stru','strs','vbudget_column','vbudget_layer','weight')
  
  for p in xrange(100):
    if p == 0:
      t = 'best_est'
    else:
      t = 'p%02i'%p
    
    tfolder = os.path.join(folder,'merge',t,'*.nc')
    #Remove the forcing.txt because it's in the netcdf-files
    os.system('rm -f %s'%(os.path.join(folder,'merge',t,'*.txt')))
    #Find all netcdf-files that have been copied into merge (from the first period)
    for tfile in glob.glob(tfolder):
      ncfile = os.path.basename(tfile)
      sys.stdout.write('Merging %s in %s \n'%(ncfile,t))
      #Make the time dimension a record dimension, so that stuff can be added
      os.system('ncks -O --mk_rec_dmn time %s %s'%(tfile,os.path.join(folder,'merge',t,'tmp.nc')))
      os.system('mv %s %s'%(os.path.join(folder,'merge',t,'tmp.nc'),tfile))
      #Open the file
      with nc(tfile,'a') as h5:
        #Fix the time units
        h5.variables['time'].units='seconds since 1970-01-01 00:00:00'
        if 'time_offset' in h5.variables.keys():
          h5.variables['time_offset'].units='seconds since 1970-01-01 00:00:00'
        #Cycle through all periods
        for start,end,present in dates:
          if present:
            #For this period we have data, read it an add it to the target file
            tstring='%s-%s'%(start.strftime('%Y%m%d'),end.strftime('%Y%m%d'))
            sfile = os.path.join(folder,tstring,t,ncfile)
            concanate(sfile,h5,exclude,start)
          else:
            #This period does not have data, add missing data to the target file
            fill(h5,start,end,exclude)
        #Fix up the time, metadata
        fix_time(h5,dates[0][0],dates[-1][1],exclude)
#    break
def main(folder,dates):
  starts=[]
  ends=[]
  for d in dates:
      #Get the dates the were given as inputs
      s,e=d.split(',')[0:2]
      starts.append(datetime.strptime(s,'%Y%m%d_%H%M'))
      ends.append(datetime.strptime(e,'%Y%m%d_%H%M'))
  #Find out for which periods we have data and for which not
  periods = get_periods(starts,ends)
  #Copy the files for the first period
  copy(folder,periods[0])
  #And merge it with the rest
  merge(folder,periods[1:])



if __name__== '__main__':
  """ This module merges the data into one single output file """
  try:
    folder=sys.argv[1]
    dates=sys.argv[2:]
  except IndexError:
    sys.stderr.write('Usage: %s va_output dates'%sys.argv[0])
    sys.exit(257)
  main(folder,dates)
