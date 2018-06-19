from netCDF4 import Dataset as nc
from netCDF4 import date2num,num2date
import numpy as np
from datetime import datetime
import sys,os,re

class Meta(dict):
    def __getattr__(self, name):
        try:
            return self[name]
        except KeyError as e:
            raise AttributeError(e)
    def __setattr__(self, name, value):
        self[name] = value

def getlon(c,dw):
    R=6370.9989
    # check if dw in m or km
    if np.fabs(dw) >= 1000:
        R*= 1000.
    return np.round(c + (dw/(R*np.cos(c*np.pi/180.)) * 180./np.pi),3)

def getlat(c,dw):
    R=6370.9989
    # check if dw in m or km
    if np.fabs(dw) >= 1000:
        R*= 1000.
    return np.round(c + (dw/R * 180./np.pi),3)
class NC(object):
    def __init__(self,infile,metadata=None,units='seconds since 1970-01-01 00:00:00Z'):
        '''
            Class to creat 10 minute netcdfiles from arbitry netcdf files
            as input.

            Instances:
                infile (str-object) : The filename of containing the input
                                      data
                metadata (NC-object) : A class containing the metadata
                                       None if not assigned yet

        '''
        self.infile = infile
        self.dirname = os.path.join(os.path.dirname(os.path.abspath(infile)),'new')
        if type(metadata) == type(None):
            # No metadata has been assigned yet, create it
            self.metadata = self.get_metadata(self.infile)
        else:
            self.metadata = metadata
    @classmethod 
    def get_metadata(cls,infile,units='seconds since 1970-01-01 00:00:00Z'):
        """
            Method to creat meta data from a netcdf file

            Variables:
                infile (str-object) : The filename that contains the meta data

            Returns:
                dict-object : A dictionary with all metadata
        """
        metadata={}
        #with nc(infile,'r') as s:
        print(infile)
        s = nc(infile,'r')
        #Get the lon info
        metadata['lon']=Meta(
                dim = (len(s.dimensions['x']),),
                dim_name = 'lon',
                dims=('i','j'),
                long_name = 'longitude',
                units = 'degrees_east',
                axis = 'X',
                data = s.variables['x'][:])
        metadata['j']=Meta(
                dim = (len(s.dimensions['x']),),
                dim_name = 'j',
                dims=('j',),
                long_name = 'longitude',
                units = 'degrees_east',
                axis = 'X',
                data = s.variables['x'][:])
        metadata['i'] = Meta(
                dim = (len(s.dimensions['y']),),
                dims = ('i',),
                long_name = 'longitude',
                units = 'degrees_east',
                axis = 'Y',
                data = s.variables['y'][:])
        metadata['lat'] = Meta(
                dim = (len(s.dimensions['y']),),
                dim_name = 'lat',
                dims = ('i','j'),
                long_name = 'longitude',
                units = 'degrees_east',
                axis = 'Y',
                data = s.variables['y'][:])
        metadata['time'] = Meta(
                dim = (None,),
                dims = ('time',),
                long_name = 'time of radar scan',
                units = s.variables['time'].units,
                data= s.variables['time'][:],
                axis = 'T')
        metadata['rain_rate'] = Meta(
                long_name = 'rain rate',
                dims = ('i','j'),
                axis = 'lonlat')
        for varname in ('rr','rain_rate','radar_estimated_rain_rate'):
            try:
                #Get the rain rates
                metadata['rain_rate'].units = s.variables[varname].units
                metadata['rain_rate'].varname=varname
                break
            except KeyError:
                pass
        try:
            center=(s.variables['radar_latitude'][:],s.variables['radar_longitude'][:])
        except KeyError:
            center=(-12.24919987,131.04440308)

        
        #Correct some of the metadata

        #Like time
        dates = num2date(metadata['time'].data,metadata['time'].units)
        metadata['time'].units=units #correct the uints
        metadata['time'].data=date2num(dates,units)
        #Correct the Lon/Lats
        #Get the width of the radar and the corresponding lon/lats
        xw=(metadata['j'].data[0],metadata['j'].data[-1])
        lon=np.flipud(np.array([getlon(center[1],d) for d in metadata['j'].data]))
        yw=(metadata['i'].data[0],metadata['i'].data[-1])
        lat=np.array([getlat(center[0],d) for d in metadata['i'].data])
        
        Y,X=np.meshgrid(lat,lon)
        metadata['lat'].data = Y
        metadata['lon'].data = X
        metadata['i'].data = lat
        metadata['j'].data=lon
        


        cls.source=s
        return metadata


    @staticmethod
    def create_metadata(T,metadata,tstep):
        '''
            Method to create metadata

            Variables:
                T (netcdf4-object) : The empty netcdf object that should be 
                                     created
                metadata (dict-obj) : The dictionary containing the metadata
        '''
        for f in ('i','j','lat','lon','time'):
            T.createDimension(f,metadata[f].dim[0])
            if f == 'time':
                dtype='i'
                data = np.array([metadata[f].data[tstep]])
            else:
                dtype='f'
                data = metadata[f].data
            T.createVariable(f,dtype,metadata[f].dims)
            for attr in ('units','axis','long_name'):
                setattr(T.variables[f],attr,metadata[f][attr])
            T.variables[f][:]=data

        T.createVariable('rain_rate','f',('i','j'),fill_value=-9999.0)
        T.variables['rain_rate'].long_name='rain rate'
        T.variables['rain_rate'].units='mm/hr'
        T.variables['rain_rate'].missing_value=-9999.0

        return T


    def create_ncfile(self,tstep,head):
        '''
            NC Ginstance to create a netcdf outputfile for a given time step

            Variables:
                tstep (int-object) = the index of the timestep
        '''

        #Get the timestep and convert it to a string
        ts = self.metadata['time'].data[tstep]
        date_string = num2date(ts,self.metadata['time'].units).strftime('%Y%m%d_%H%M.nc')
        fname = os.path.join(self.dirname,head+date_string)
        with nc(fname,'w') as t:
            self.create_metadata(t,self.metadata,tstep)
            #Get the source
            r_source=np.ma.masked_less(self.source.variables[
                    self.metadata['rain_rate'].varname][tstep],0.1).filled(0)
            maskf = os.path.join(os.path.dirname(sys.argv[0]),'ring_mask.npz')
            mask = np.ma.masked_less(np.load(maskf)['mask'],-1)
            r_source=np.ma.masked_invalid(r_source * mask)
            r_source=np.ma.masked_greater(r_source,300.)
            t.variables['time'][:]=self.metadata['time'].data[tstep]

            try:
                t.variables['rain_rate'][:]=r_source.filled(-9999.0)
            except AttributeError:
                t.variables['rain_rate'][:]=r_source

        os.chmod(fname, 0o777)

if __name__ == '__main__':
    try:
        infile=sys.argv[1]
        outfile=sys.argv[2]
    except IndexError:
        sys.exit('Error: \n Usage: %s infile.nc outfile.nc' %sys.argv[0])
    try:
        head=sys.argv[3]
    except IndexError:
        fn = os.path.basename(outfile).replace('.nc','').replace('.nc4','').replace('-','_')
        #head = fn.replace(re.search(r'\d{8}_\d{4}', fn).group(),'')
        head = fn.replace(re.search(r'\d{8}', fn).group(),'')

    #Open the infile and outfile
    if not os.path.isfile(infile):
        sys.stderr.write("%s is missing"%infile)
    #Get the metadata
    Meta = NC(infile)
    for tt in range(len(Meta.metadata['time'].data)):
        Meta.create_ncfile(tt,head)
    Meta.source.close()
    sys.stdout.write('%s: created netcdffiles from %s\n' %(sys.argv[0],infile))
