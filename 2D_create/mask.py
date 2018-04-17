from netCDF4 import Dataset as nc
#import Polygon
import numpy as np

def create(out,lons,lats):

    f = nc(out,'w',format='NETCDF3_CLASSIC')

    f.createDimension('longitude',len(lons))
    f.createDimension('latitude',len(lats))

    f.createVariable('latitude','f',('latitude'))
    f.createVariable('longitude','f',('longitude'))
    #f.createVariable('lat','f',('latitude','longitude'))
    #f.createVariable('lon','f',('latitude','longitude'))
    f.createVariable('masking_var','f',('latitude','longitude'))

    f.variables['latitude'][:]=lats
    f.variables['longitude'][:]=lons
    #f.variables['lat'][:]=np.meshgrid(lats,lons)
    #f.variables['lon'][:]=np.meshgrid(lons,lats)

    f.variables['latitude'].units='degrees_north'
    f.variables['latitude'].axis='Y'


    f.variables['longitude'].units='degrees_east'
    f.variables['longitude'].axis='X'

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



def creatpoly(poly,lons,lats,mask=-99.00):
    
    polycheck=np.vectorize(point_inside_polygon,excluded=['poly'])
    x,y = np.meshgrid(lons,lats)

    grid = (polycheck(x,y,poly=poly).astype(np.int8))
    grid = np.ma.masked_not_equal(grid,1).filled(mask)

    
    return grid.astype(np.int8)

if __name__ == "__main__":

    import sys
    lons,lats = 0,0
    res=[0.5,0.5]
    mask=-99.0
    polyx = [131.7651,131.7609,131.1355,129.8,130.41669]
    polyy = [-11.3081,-12.5858,-13.2287,-12.4,-11.40891]
    out = 'mask.nc'
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
        lons=np.arange(129.5,133,float(res[0]))
    else:
        lons=np.arange(float(lons[0]),float(lons[1])+float(res[0]),float(res[0]))

    if type(lats) == type(0):
        if len(res) == 1:
            lats=-1*np.arange(11.,14.5,float(res[0]))
        else:
            lats=-1*np.arange(11.,14.5,flaot(res[1]))
    else:
        if len(res) == 1:
            lats=np.arange(float(lats[0]),float(lats[1]),float(res[0]))
        else:
            lats=np.arange(float(lats[0]),float(lats[1]),float(res[1]))
    poly=[]
    for i in xrange(len(polyx)):
        if len(polyx) and len(polyy) and len(str(polyx[i])) and len(str(polyy[i])):
            poly.append((float(polyx[i]),float(polyy[i])))
    f = create(out,lons,lats)
    

    
    f.variables['masking_var'].missing_value=mask
    f.variables['masking_var'][:]=creatpoly(poly,lons,lats,mask=mask)
    f.close()





