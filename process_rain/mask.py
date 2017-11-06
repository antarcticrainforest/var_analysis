#!/usr/bin/env python2

from netCDF4 import Dataset as nc
#import Polygon
import numpy as np

def create(out,lons,lats):

    f = nc(out,'w',format='NETCDF3_CLASSIC')

    f.createDimension('j',len(lons))
    f.createDimension('i',len(lats))

    f.createVariable('lat','f',('i','j'))
    f.createVariable('lon','f',('i','j'))
    f.createVariable('i','f',('i',))
    f.createVariable('j','f',('j',))
    #f.createVariable('lat','f',('latitude','longitude'))
    #f.createVariable('lon','f',('latitude','longitude'))
    f.createVariable('rain_rate','f',('i','j'))

    X,Y=np.meshgrid(lons,lats)
    f.variables['i'][:]=lats
    f.variables['j'][:]=lons
    f.variables['lon'][:]=X
    f.variables['lat'][:]=Y

    f.variables['i'].units='degrees_north'
    f.variables['i'].axis='Y'


    f.variables['j'].units='degrees_east'
    f.variables['j'].axis='X'

    return f

def point_inside_polygon(x,y,poly=[(1,2),(3,3),(4,1)]):

        n = len(poly)
        inside =False

        p1x,p1y = poly[0]
        for i in range(n+1):
            p2x,p2y = poly[i % n]
            if y > min(p1y,p2y):
                if y <= max(p1y,p2y):
                    if x <= max(p1x,p2x):
                        if p1y != p2y:
                            xinters = (y-p1y)*(p2x-p1x)/(p2y-p1y)+p1x
                        if p1x == p2x or x <= xinters:
                            inside = not inside
            p1x,p1y = p2x,p2y

        return inside



def creatpoly(poly,lons,lats,mask=-9999.0):
    
    polycheck=np.vectorize(point_inside_polygon,excluded=['poly'])
    x,y = np.meshgrid(lons,lats)

    grid = (polycheck(x,y,poly=poly).astype(np.int8))
    grid = np.ma.masked_not_equal(grid,1).filled(mask)

    
    return grid.astype(np.int8)

if __name__ == "__main__":

    import sys,os
    try:
      seas = sys.argv[1]
    except IndexError:
      sys.stderr.write('Usage: %s season [YYYY] '%(sys.argv[0]))
      sys.exit(257)
    lons,lats = 0,0
    res=[0.02299,0.02246]
    mask=-99.0
    polyx = [131.7651,131.7609,131.1355,129.8,130.41669]
    polyy = [-11.3081,-12.5858,-13.2287,-12.4,-11.40891]
    out = os.path.join(os.path.dirname(__file__),'mask_%s.nc' %seas)
    try:
        if '-h' in sys.argv[1]:
            sys.exit("Usage: %s --lon=lons1,..,lonsN --lat=lats1,..,latsN --polyx=polyx1,..,polyxN, --polyy=polyy1,..,polyyN" %sys.argv[0])
        for arg in sys.argv[1:]:
            if arg.lower().startswith('--lon'):
                lons=np.array(arg.split('=')[1].split(',')).astype(np.float32)
            if arg.lower().startswith('--lat'):
                lats=np.array(arg.split('=')[1].split(',')).astype(np.float32)
            if arg.lower().startswith('--polyy'):
                polyy=arg.split('=')[1].split(',')
            if arg.lower().startswith('--polyx'):
                polyx=arg.split('=')[1].split(',')
            if arg.lower().startswith('--out'):
                out=arg.split('=')[1]
            if arg.lower().startswith('--res'):
                res=arg.split('=')[1].split(',')
            if arg.lower().startswith('--mask'):
                mask=float(arg.split('=')[1])

    except IndexError:
        pass
    if type(lons) == type(0):
        lons=np.arange(129.6655,132.4233+float(res[0]),float(res[0]))
    else:
        lons=np.arange(float(lons[0]),float(lons[1])+float(res[0]),float(res[0]))

    if type(lats) == type(0):
        if len(res) == 1:
            lats=-1*np.arange(10.9017,13.5967+float(res[0]),float(res[0]))
        else:
            lats=-1*np.arange(10.9017,13.5967+float(res[1]),float(res[1]))
    else:
        if len(res) == 1:
            lats=np.arange(float(lats[0]),float(lats[1]),float(res[0]))
        else:
            lats=np.arange(float(lats[0]),float(lats[1]),float(res[1]))
    poly=[]
    for i in xrange(len(polyx)):
        if len(polyx) and len(polyy) and len(str(polyx[i])) and len(str(polyy[i])):
            poly.append((float(polyx[i]),float(polyy[i])))
    lats=lats[...,::-1]
    f = create(out,lons,lats)
    

    
    f.variables['rain_rate'].missing_value=mask
    f.variables['rain_rate'][:]=creatpoly(poly,lons,lats,mask=mask)
    f.close()





