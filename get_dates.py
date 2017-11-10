import datetime,os,glob,sys, numpy as np
from datetime import timedelta

def lookup(f,head):
    """
        Get the days in a season
    """
    if head.lower() == 'cpol':
      s = '_'.join(f.replace('-','_').replace('.','_').split('_')[-3:-2])
    else:
      s = '_'.join(f.replace('-','_').replace('.','_').split('_')[-3:-2])
    return datetime.datetime.strptime(s,'%Y%m%d')



def get_filenames(folder,head):

    """
        Get all filenames for the months in the season
    """
    if head.lower() == 'cpol':
      files=glob.glob(os.path.join(folder,'*_????????_*'))
    else:
      files=glob.glob(os.path.join(folder,'*%s*'%head.lower()))
    dates=np.array([lookup(f,head) for f in files])
    dates.sort()
    return dates
    return MIN,MAX,np.unique(np.array([d.strftime('%Y%m'+'01') for d in dates]))

def get_months(dates):
  for start,end in dates:
    mon = []
    tmp = start
    dt = timedelta(days=1)
    while tmp <= end:
      if tmp.strftime('%Y%m01') not in mon:
        mon.append(tmp.strftime('%Y%m01'))
      tmp += dt
    sys.stdout.write('%s,%s,%s\n'\
        %(start.strftime('%Y%m%d_0000'),end.strftime('%Y%m%d_1800'),','.join(mon)))

def split(dates):
  first,last = dates[0],dates[-1]
  dt = dates[1] - dates[0]
  out = []
  tmp = first
  missing,missing_all = [],[]
  nn = -1
  while tmp <= last:
    if not tmp in dates:
      out.append((first,(tmp-dt)))
      while tmp not in dates and tmp <= last:
        tmp += dt
      first = tmp
    tmp += dt
  return get_months(out)


def main(cpoldir):
  dates={}
  for key,value in dict(cpol=cpoldir).iteritems():
    dates[key]=get_filenames(value,key)
  out =[]
  maxl = 0
  for v in dates['cpol']:
    if v in dates['cpol']:
      out.append(v)
  if len(out) == 0:
    sys.exit(257)
  return(split(np.array(out)))

if __name__ == '__main__':
    import sys

    try:
      raininput=sys.argv[1]
    except IndexError, ValueError:
        sys.exit('Usage: \t python %s CPOL-INPUTDIR'%(sys.argv[0]))
    

    main(raininput)




