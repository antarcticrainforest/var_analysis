#!/usr/bin/env python2

import numpy as np
from netCDF4 import date2num
from netCDF4 import Dataset as nc
from datetime import datetime
import sys


def main(infile,outfile,maskfile,best_est_only):

    mask=nc(maskfile).variables['rain_rate'][:]
    i=nc(infile)
    f=nc(outfile,'w')

    f.createDimension('time',None)
    f.createVariable('time','f',('time',))
    time = "_".join(infile.replace("domavg","").split('_')[-3:-1])
    date=datetime.strptime(time,"%Y%m%d_%H%M")
    num=date2num(date,'minutes since 1970-01-01 00:00:00')
    f.variables['time'][:]=np.array([num])
    f.variables['time'].units='minutes since 1970-01-01 00:00:00'
    f.variables['time'].long_name='time of radar scan'
    f.variables['time'].axis='T'

    f.createVariable('rain_rate','f',('time',),fill_value=-99.99)
    prate = np.ma.masked_invalid(i.variables['rain_rate'][:])
    prate = np.ma.masked_outside(prate,-1,1000.)
    try:
        f.variables['rain_rate'][:]=np.mean(prate*mask)
    except ValueError:
        mask=mask[2:-2,2:-2]

        try:
            f.variables['rain_rate'][:]=np.mean(prate*mask)
        except IndexError:
            f.variables['rain_rate'][:]=np.mean(prate*mask).filled(-99.99)
        
    f.variables['rain_rate'].units='mm/hr'
    f.variables['rain_rate'].long_name='Domain avg. rain rate'
    f.variables['rain_rate'].missing_value=-99.99

    if best_est_only != 1:
        prate=np.ma.masked_invalid(i.variables['rain_rate_pdf'][:])
        prate=np.ma.masked_outside(prate,-1,1000.)
        f.createDimension('percentile',100)
        f.createVariable('percentile','f',('percentile',))
        f.createVariable('rain_rate_pdf','f',('time','percentile'),fill_value=-99.99)
        f.variables['percentile'][:]=i.variables['percentile'][:]
        f.variables['percentile'].units=i.variables['percentile'].units
        f.variables['percentile'].long_name=i.variables['percentile'].long_name
        f.variables['rain_rate_pdf'].units='mm/hr'
        f.variables['rain_rate_pdf'].long_name='Domain avg. pdf rain rate'
        f.variables['rain_rate_pdf'].missing_value=-99.99

        s1=prate*mask[...,np.newaxis]

        s1=np.mean(s1.T.reshape(s1.T.shape[0],-1),axis=-1)
        f.variables['rain_rate_pdf'][:]=np.array([s1])

    i.close()
    f.close()

if __name__ == "__main__":
    
    try:
        infile=sys.argv[1]
        outfile=sys.argv[2]
        maskfile=sys.argv[3]
        best_est_only=int(sys.argv[4])
    except IndexError:
        sys.exit("Not enough cmd arguments given")
    main(infile,outfile,maskfile,best_est_only)
