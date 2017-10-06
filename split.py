import numpy as np
import sys



if __name__ == '__main__':

    try:
        ntask=int(sys.argv[1])
        dates=np.array(sys.argv[2:])
    except (IndexError,ValueError,TypeError):
        sys.exit('Usage: %s ntasks dates'%sys.argv[0])


    ary=np.array_split(dates,ntask)
    for a in list(ary):
        print ','.join(list(a))
