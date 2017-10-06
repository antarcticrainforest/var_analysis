from netCDF4 import Dataset as nc, date2num
from datetime import datetime
import numpy as np
import glob, os




def main(folder,tstring):

    #Get filnames 
    fname = glob.glob(os.path.join(folder,'*_????????_????_*.nc'))[0]
    fn = fname.replace('.nc4','').replace('.nc','').replace('-','_').replace('.','_').split('_')
    tstring2='_'.join(fn[-4:-2])
    fname2 = fname.replace(tstring2,tstring)
    date = datetime.strptime(tstring,'%Y%m%d_%H%M')
    
    os.system('cp %s %s' %(fname,fname2))
    with nc(fname2,'a') as f:
        nn = date2num(date,f.variables['time'].units)
        f.variables['num_radar_scans'][:] = np.array([0])
        f.variables['num_radar_scans'].input_files=''
        f.variables['time'][:] = np.array([nn])
        f.variables['rain_rate'][:]=np.array([f.variables['rain_rate'].missing_value])
        f.variables['rain_rate_pdf'][:]=np.ones([100])*f.variables['rain_rate_pdf'].missing_value







if __name__ == '__main__':
    import sys

    try:
        folder,tstring = sys.argv[1:3]
    except (IndexError,ValueError):
        sys.exit('Usage : %s rain_input_folder YYYYMMDD_HHMM')

    main(folder,tstring)
