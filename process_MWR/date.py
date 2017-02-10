import datetime,os,sys,calendar

def main(start,end):
    years= []
    months =[]
    first=[]
    last=[]
    check=[]
    t=datetime.datetime.strptime(start,'%Y%m%d_%H%M')
    end=datetime.datetime.strptime(end,'%Y%m%d_%H%M')
    dt = datetime.timedelta(days=1)
    Dir = os.path.dirname(sys.argv[0])
    while t <= end:
        c= '%04i_%02i'%(t.year,t.month)
        if c not in check:
            check.append(c)
            years.append("'%04i'"%t.year)
            months.append("'%02i'"%t.month)
            first.append("%02i"%t.day)
            last.append("%02i"%calendar.monthrange(t.year,t.month)[1])
        t+=dt
    last[-1]='%02i'%end.day

    for f,d in zip(('.years','.months','.first','.last'),(years,months,first,last)):
        with open(os.path.join(Dir,f),'w') as t:
            t.write(','.join(d)+'\n')


if __name__ == '__main__':
    try:
        main(*sys.argv[1:3])
    except IndexError:
        sys.exit('Usage %s start_date end_date'%sys.argv[0])
