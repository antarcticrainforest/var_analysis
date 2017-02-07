import datetime,os,glob,sys
import numpy as np

def lookup(f):
    """
        Get the days in a season
    """
    s = '_'.join(f.replace('-','_').replace('.','_').split('_')[-3:-1])
    return datetime.datetime.strptime(s,'%Y%m%d_%H%M')
    
def get_filenames(folder,months=range(1,13)):
    """
        Get all filenames for the months in the season
    """
    rainfiles=glob.glob(os.path.join(folder,'*_????????_????.*'))
    dates=np.array([lookup(f) for f in rainfiles])
    MIN,MAX=dates.min().strftime('%Y%m%d'),dates.max().strftime('%Y%m')+'01'
    if MIN == MAX:
        return MIN
    #else
    
def main(folder):
    """
    Get the start and the end of days that have CPOL data within a rain season

    Input:
        folder (str-object): The input directory of the rain data
    Returns:
        days (str-object): String with all days withing the season
    """
    
    return ' '.join(get_filenames(folder))


if __name__ == '__main__':
    import sys

    try:
        folder=sys.argv[1]
    except IndexError:
        sys.exit('Usage: \t python %s INPUTDIR '%(sys.argv[0]))


    sys.stdout.write('%s\n'%main(folder))
