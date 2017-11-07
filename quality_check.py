#!/usr/bin/env python2

from netCDF4 import Dataset as nc, num2date, date2num
#import pandas as pd
import sys, os,glob
from datetime import datetime, timedelta


class ECMWF(object):
    ''''
        Provide the ecmwf analysis data
    '''
    def __init__(self,folder):
        self.filenames=(os.path.join(folder,'2D_put','ecmwf.nc'),
                os.path.join(folder,'3D_put','surf_p.nc'),
                os.path.join(folder,'3D_put','upa_p.nc'))



class MWR(object):
    '''
        Provide the micro wave radiometer data
    '''
    def __init__(self,folder):
        self.filenames=(os.path.join(folder,'MWR-DATA','mwrlos_6h_interp.nc'),)
class CPOL(object):
    '''
        Provide the radar rain rate estimates
    '''
    def __init__(self,folder):
        self.filenames=[]
        for ii in xrange(-1,100):
            if ii == -1:
                perc='pret_06h_NOGAUGE_*'
            else:
                perc='pret_06h_p%02i_*'%ii
            fn=glob.glob(os.path.join(folder,'radar_rain',perc))[0]
            self.filenames.append(fn)

def my_format(header,value,num):
    if header != 'Missing value':
        try:
            value=float(value)
            t='%.2f'%value
        except:
            t='%s'%value
    else:
            t='%s'%value

    fmt = ' %s'%t
    tl = len(fmt)
    th = len(header)
    delta = th - tl
    if num==1:
        dt=1
        fmt+=' '*(delta+1)
    else:
        dt=2
    
    space=' '*(delta+dt)
    fmt= ' '+space+t
    return fmt+'|'

class Check(MWR,CPOL,ECMWF):

    ''''
        Meta-class to do some quality checking,
    '''

    def __init__(self,cls,folder):
         #super( cls).__init__(name, bases, dct)
         cls.__init__(self,folder)
         self.missing = {}
         self.data = {}
         self.check()

    
    def summary(self):
        varname='Variable name'
        missingname='Missing value'
        meanname='Mean value'
        stdname='Std value'
        minname='Min value'
        maxname='Max value'
        head1='%s | %s | %s | %s | %s | %s'%(varname,missingname,meanname,stdname,minname,maxname)
        print head1
        print '-'*len(head1)+'--'

        for var in self.missing.keys():
            vv=var.split('-')[0]
            thisline=''
            num = 1
            for vh,vv in ((varname,vv),(missingname,self.missing[var]),
                    (meanname,self.data[var][0]),(stdname,self.data[var][1]),
                    (minname,self.data[var][2]),(maxname,self.data[var][3])):
                thisline+=my_format(vh,vv,num)
                num+=1

            print thisline

    @staticmethod
    def check_time(ncf,fn):
        df = 6
        times=num2date(ncf.variables['time'][:],ncf.variables['time'].units)
        tstep = times[0]
        last = times[-1]
        while tstep <= last:
            if tstep not in times:
                print tstep.strftime('Timestep %Y-%m-%d %H is missing in ')+fn
            tstep += timedelta(hours=df)
        return times
    
    def check(self):
        nn = 0
        for fn in self.filenames:
          try:
            with nc(fn) as f:
                self.time=self.check_time(f,fn)
                for var in f.variables:
                    data = f.variables[var][:]
                    try:
                        self.missing['%s-%03i'%(var,nn)] = f.variables[var].missing_value
                        self.data['%s-%03i'%(var,nn)] = (data.mean(),data.std(),data.min(),data.max())
                    except AttributeError:
                        pass
                    nn+=1
          except RuntimeError, OSError:
            print '%s not existing' %fn
            sys.exit()
if __name__ == '__main__':

    try:
        folder = sys.argv[1]
    except IndexError:
        sys.stderr.write('Usage: %s va_input\n'%sys.argv[0])
        sys.exit(257)

    ntstep = 0
    last = 0
    end = 0
    for cls in (ECMWF,MWR,CPOL):
        C = Check(cls,folder)
        print cls,'%s - %s'%(C.time[0].strftime('%Y-%m-%d %H'),C.time[-1].strftime('%Y-%m-%d %H')),len(C.time)
        C.summary()
