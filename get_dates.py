import datetime,os,glob,sys
import numpy as np

def lookup(f):
    """
        Get the days in a season
    """
    s = '_'.join(f.replace('-','_').replace('.','_').split('_')[-3:-1])
    return datetime.datetime.strptime(s,'%Y%m%d_%H%M')

def roundupdown(a,b):
    a+=datetime.timedelta(hours=24)
    b-=datetime.timedelta(hours=24)
    a=datetime.datetime(a.year,a.month,a.day,0,0)
    b=datetime.datetime(b.year,b.month,b.day,18,0)

    return a.strftime('%Y%m%d_%H%M'),b.strftime('%Y%m%d_%H%M')
    h1=(6-a.hour%6)*(int(bool(a.hour%6)))
    h2=(b.hour%6)*(int(bool(b.hour%6)))
    a+=datetime.timedelta(seconds=h1*60**2)
    b-=datetime.timedelta(seconds=h2*60**2)
    return a.strftime('%Y%m%d_%H%M'),b.strftime('%Y%m%d_%H%M')

    
def get_filenames(folder,months=range(1,13)):
    """
        Get all filenames for the months in the season
    """
    rainfiles=glob.glob(os.path.join(folder,'*_????????_????.*'))
    dates=np.array([lookup(f) for f in rainfiles])
    dates.sort()
    MIN,MAX=dates.min(),dates.max()
    return MIN,MAX,np.unique(np.array([d.strftime('%Y%m'+'01') for d in dates]))
def main(folder):
    """
    Get the start and the end of days that have CPOL data within a rain season

    Input:
        folder (str-object): The input directory of the rain data
    Returns:
        days (str-object): String with all days withing the season
    """

    Min,Max,dates = get_filenames(folder)
    Min,Max = roundupdown(Min,Max)
    return Min+' '+Max+' '+' '.join(list(dates))

    


if __name__ == '__main__':
    import sys

    try:
        folder=sys.argv[1]
    except IndexError:
        sys.exit('Usage: \t python %s INPUTDIR '%(sys.argv[0]))


    sys.stdout.write('%s\n'%main(folder))
