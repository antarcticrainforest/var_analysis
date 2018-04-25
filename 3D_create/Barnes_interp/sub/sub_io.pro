	pro ipt_t,inputfile,nv,nt,t,d

	  openr,1,inputfile
	  readf,1,nv,nt
;zhang begin for ok mesonet
;;;	  readf,1,nv,nt
;          c=' '
;          readf,1,c
;          readf,1,c
;          nv=4
;          nt=143
;zhang end for ok mesonet
	  t=fltarr(nt)
	  d=fltarr(nv,nt)
	  d1=fltarr(nv)
	  for l=0,nt-1 do begin
	    readf,1,t1,d1
	    t(l)=t1
	    d(*,l)=d1(*)
	  endfor
	  close,1

	end


	pro opt_t2,outputfile,heading,nv,nt,t,d

	  openw,1,outputfile
	  printf,1,nv,nt
          printf,1,heading
	  d1=fltarr(nv)
	  for l=0,nt-1 do begin
	    d1(*)=d(*,l)
	    printf,1,t(l),d1
	  endfor
	  close,1

	end

	pro opt_t,outputfile,nv,nt,t,d

	  openw,1,outputfile
	  printf,1,nv,nt
	  d1=fltarr(nv)
	  for l=0,nt-1 do begin
	    d1(*)=d(*,l)
	    printf,1,t(l),d1
	  endfor
	  close,1

	end


	pro ipt_ht_vk,inputfile,instrument,nv,nst,nt,v,vk,st,lon,lat,t,d
	  openr,1,inputfile
	  instrument=' '
	  c=' '
	  readf,1,format='(a20)',instrument
	  print,instrument
	  readf,1,c
	  print,c
	  readf,1,nv,nst,nt
	  print,nv,nst,nt
	  v=strarr(nv)
	  vk=intarr(nv)
	  st=strarr(nst)
	  lon=fltarr(nst)
	  lat=fltarr(nst)
	  t=fltarr(nt)
	  d=fltarr(nv,nst,nt)
	  readf,1,c
	  print,c
	  readf,1,format='(4a20)',v
	  print,v
	  readf,1,c
	  print,c
	  readf,1,vk
	  print,vk
	  readf,1,c
	  print,c
	  readf,1,format='(4a20)',st
	  print,st
	  readf,1,c
	  print,c
	  readf,1,lon
	  print,lon
	  readf,1,c
	  print,c
	  readf,1,lat
	  print,lat
	  readf,1,c
	  print,c
	  readf,1,t
	  print,t
	  readf,1,c
	  print,c
	  readf,1,d
	  close,1

	end


	pro ipt_ht,inputfile,instrument,nv,nst,nt,v,st,lon,lat,t,d
	  openr,1,inputfile
	  instrument=' '
	  c=' '
	  readf,1,format='(a20)',instrument
	  print,instrument
	  readf,1,c
	  print,c
	  readf,1,nv,nst,nt
	  print,nv,nst,nt
	  v=strarr(nv)
	  st=strarr(nst)
	  lon=fltarr(nst)
	  lat=fltarr(nst)
	  t=fltarr(nt)
	  d=fltarr(nv,nst,nt)
	  readf,1,c
	  print,c
	  readf,1,format='(4a20)',v
	  print,v
	  readf,1,c
	  print,c
	  readf,1,format='(4a20)',st
	  print,st
	  readf,1,c
	  print,c
	  readf,1,lon
	  print,lon
	  readf,1,c
	  print,c
	  readf,1,lat
	  print,lat
	  readf,1,c
	  print,c
	  readf,1,t
	  print,t
	  readf,1,c
	  print,c
	  readf,1,d
	  close,1

	end


	pro ipt_vht,inputfile,instrument,nv,np,nst,nt,v,p,st,t,d
	  openr,1,inputfile
	  instrument=' '
	  c=' '
	  readf,1,format='(a20)',instrument
	  print,instrument
	  readf,1,c
	  print,c
	  readf,1,nv,np,nst,nt
	  print,nv,np,nst,nt
	  v=strarr(nv)
	  p=fltarr(np)
	  st=strarr(nst)
; +++++++++++++++++++++++++++++++++++++++++++++11/12/98 
;            print,'warning, for TOGACOARE  nst changed from 5 to 14!'
;             if(strpos(inputfile,'/tmp/jlin/') ge 0)then begin
;               if(nst eq 5)then st=strarr(14) 
;             endif
;+++++++++++++++++++++++++++++++++++++++++++++++
	  t=fltarr(nt)
	  d=fltarr(nv,np,nst,nt)
	  readf,1,c
	  print,c
	  readf,1,format='(4a20)',v
	  print,v
	  readf,1,c
	  print,c
	  readf,1,p
	  print,p
	  readf,1,c
	  print,c
	  readf,1,format='(4a20)',st
	  print,st
	  readf,1,c
	  print,c
	  readf,1,t
	  print,'t(0),t(1),t(nt-1)'
	  print,t(0),t(1),t(nt-1)
	  readf,1,c
	  print,c
	  readf,1,d
	  close,1

	end


	pro ipt_grid_vk,inputfile,instrument,nv,nlon,nlat,nt,v,vk,lon,lat,t,d

        openr,1,inputfile
        instrument=' '
        c=' '
        readf,1,format='(a20)',instrument
        print,instrument
        readf,1,c
        print,c
        readf,1,nv,nlon,nlat,nt
        print,nv,nlon,nlat,nt
	v=strarr(nv)
	vk=intarr(nv)
        lon=fltarr(nlon)
        lat=fltarr(nlat)
        t=fltarr(nt)
        d=fltarr(nv,nlon,nlat,nt)
        readf,1,c
        print,c
        readf,1,format='(4a20)',v
        print,v
	readf,1,c
	print,c
	readf,1,vk
	print,vk
	readf,1,c
	print,c
        readf,1,lon
        print,lon
        readf,1,c
        print,c
        readf,1,lat
        print,lat
        readf,1,c
        print,c
        readf,1,t
        print,t
        readf,1,c
        print,c
        readf,1,d
        close,1

	end


	pro ipt_grid,inputfile,instrument,nv,np,nlon,nlat,nt,v,p,lon,lat,t,d

        openr,1,inputfile
        instrument=' '
        c=' '
        readf,1,instrument
        print,instrument
        readf,1,c
        print,c
        readf,1,nv,np,nlon,nlat,nt
        print,nv,np,nlon,nlat,nt
	v=strarr(nv)
	p=fltarr(np)
        lon=fltarr(nlon)
        lat=fltarr(nlat)
        t=fltarr(nt)
        d=fltarr(nv,np,nlon,nlat,nt)
        readf,1,c
        print,c
        readf,1,format='(4a20)',v
        print,v
        readf,1,c
        print,c
        readf,1,p
        print,p
        readf,1,c
        print,c
        readf,1,lon
        print,lon
        readf,1,c
        print,c
        readf,1,lat
        print,lat
        readf,1,c
        print,c
        readf,1,t
        print,t
        readf,1,c
        print,c
        readf,1,d
        close,1

	end


	pro ipt_grid1,inputfile,instrument,nv,nlon,nlat,nt,v,lon,lat,t,d

        openr,1,inputfile
        instrument=' '
        c=' '
        readf,1,instrument
        print,instrument
        readf,1,c
        print,c
        readf,1,nv,nlon,nlat,nt
        print,nv,nlon,nlat,nt
	v=strarr(nv)
        lon=fltarr(nlon)
        lat=fltarr(nlat)
        t=fltarr(nt)
        d=fltarr(nv,nlon,nlat,nt)
        readf,1,c
        print,c
        readf,1,format='(4a20)',v
        print,v
        readf,1,c
        print,c
        readf,1,lon
        print,lon
        readf,1,c
        print,c
        readf,1,lat
        print,lat
        readf,1,c
        print,c
        readf,1,t
        print,t
        readf,1,c
        print,c
        readf,1,d
        close,1

	end


	pro ipt_gridc,inputfile,nv,nlon,nlat,nt,v,lon,lat,t,d

        openr,1,inputfile
        ;instrument=' '
        c=' '
        ;readf,1,instrument
        ;print,instrument
        ;readf,1,c
        ;print,c
        nv=0 & nlon=0 & nlat=0 & nt=0
        readf,1,nv,nlon,nlat,nt
        print,nv,nlon,nlat,nt
	v=strarr(nv)
        lon=fltarr(nlon)
        lat=fltarr(nlat)
        t=fltarr(nt)
        d=fltarr(nv,nlon,nlat,nt)
        ;readf,1,c
        ;print,c
        readf,1,format='(4a20)',v
        print,v
        ;readf,1,c
        ;print,c
        readf,1,lon
        print,lon
        ;readf,1,c
        ;print,c
        readf,1,lat
        print,lat
        ;readf,1,c
        ;print,c
        readf,1,t
        print,t
        ;readf,1,c
        ;print,c
        readf,1,d
        close,1

	end

        pro opt_gridc,outputfile,nv,nlon,nlat,nt,v,lon,lat,t,d
          openw,1,outputfile
          printf,1,nv,nlon,nlat,nt
          printf,1,format='(4a20)',v
          printf,1,lon
          printf,1,lat
          printf,1,t
          printf,1,format='(4(e13.5,1x))',d
          close,1

        end


	pro opt_ht,outputfile,instrument,nv,nst,nt,v,st,lon,lat,t,d
	  openw,1,outputfile
	  printf,1,instrument
	  printf,1,'nv,nst,nt'
	  printf,1,nv,nst,nt
	  printf,1,'v(nv)'
	  printf,1,format='(4a20)',v
	  printf,1,'st(nst)'
	  printf,1,format='(4a20)',st
	  printf,1,'lon(nst)'
	  printf,1,lon
	  printf,1,'lat(nst)'
	  printf,1,lat
	  printf,1,'t(nt)'
	  printf,1,t
	  printf,1,'d(nv,nst,nt)'
	  printf,1,format='(4(e13.5,1x))',d
	  close,1

	end

	pro opt_ht_vk,outputfile,instrument,nv,nst,nt,v,vk,st,lon,lat,t,d
	  openw,1,outputfile
	  printf,1,instrument
	  printf,1,'nv,nst,nt'
	  printf,1,nv,nst,nt
	  printf,1,'v(nv)'
	  printf,1,format='(4a20)',v
	  printf,1,'vk(nv)'
	  printf,1,vk
	  printf,1,'st(nst)'
	  printf,1,format='(4a20)',st
	  printf,1,'lon(nst)'
	  printf,1,lon
	  printf,1,'lat(nst)'
	  printf,1,lat
	  printf,1,'t(nt)'
	  printf,1,t
	  printf,1,'d(nv,nst,nt)'
	  printf,1,format='(4(e13.5,1x))',d
	  close,1

	end


	pro opt_vht,outputfile,instrument,nv,np,nst,nt,v,p,st,t,d
	  openw,1,outputfile
	  printf,1,instrument
	  printf,1,'nv,np,nst,nt'
	  printf,1,nv,np,nst,nt
	  printf,1,'v(nv)'
	  printf,1,format='(4a20)',v
	  printf,1,'p(np)'
	  printf,1,p
	  printf,1,'st(nst)'
	  printf,1,format='(4a20)',st
	  printf,1,'t(nt)'
	  printf,1,t
	  printf,1,'d(nv,np,nst,nt)'
	  printf,1,format='(4(e13.5,1x))',d
	  close,1

	end


	pro opt_grid,outputfile,instrument,nv,nlon,nlat,nt,v,lon,lat,t,d
	  openw,1,outputfile
	  printf,1,instrument
	  printf,1,'nv,nlon,nlat,nt'
	  printf,1,nv,nlon,nlat,nt
	  printf,1,'v(nv)'
	  printf,1,format='(4a20)',v
	  printf,1,'lon(nlon)'
	  printf,1,lon
	  printf,1,'lat(nlat)'
	  printf,1,lat
	  printf,1,'t(nt)'
	  printf,1,t
	  printf,1,'d(nv,nlon,nlat,nt)'
	  printf,1,format='(4(e13.5,1x))',d
	  close,1

	end

	pro ipt_prof,inputfile,instrument,nv,np,nst,nt,v,stn,lat, $
           lon,alt,ht_low,ht_hi,t,d
	  openr,1,inputfile
	  instrument=' '
	  c=' '
	  readf,1,format='(a20)',instrument
	  print,instrument
	  readf,1,c
	  print,c
	  readf,1,nv,np,nst,nt
	  print,nv,np,nst,nt

	  v=strarr(nv)
	  stn=fltarr(nst)
          lat=fltarr(nst)
          lon=fltarr(nst)
          alt=fltarr(nst)
          ht_low=fltarr(np)
          ht_hi=fltarr(np)
	  t=fltarr(nt)
	  d=fltarr(nv,np,nst,nt)

	  readf,1,c
	  print,c
	  readf,1,format='(4a20)',v
	  print,v
	  readf,1,c
	  print,c
	  readf,1,stn
	  readf,1,c
	  print,c
	  readf,1,lat
	  readf,1,c
	  print,c
	  readf,1,lon
	  readf,1,c
	  print,c
	  readf,1,alt
	  readf,1,c
	  print,c
	  readf,1,ht_low
	  readf,1,c
	  print,c
	  readf,1,ht_hi
	  readf,1,c
	  print,c
	  readf,1,t
	  print,'t(0),t(1),t(nt-1)'
	  print,t(0),t(1),t(nt-1)
	  readf,1,c
	  print,c
	  readf,1,d
	  close,1

	end

        pro ipt_ruc,inputfile,instrument,nv,nlon,nlat,nlev,nt,v,lon,lat,$
                    pres,t,ktime,ps,hgt,temp,rh,uu,vv
          openr,1,inputfile
          instrument=' '
          c=' '
          readf,1,format='(a20)',instrument
          print,instrument
          readf,1,c
          print,c
          readf,1,nv,nlon,nlat,nlev,nt
          print,nv,nlon,nlat,nlev,nt

          v=strarr(nv)
          lon=fltarr(nlon,nlat)
          lat=fltarr(nlon,nlat)
          pres=fltarr(nlev)
          t=fltarr(nt)

          readf,1,c
          print,c
;xie for twpice
          readf,1,format='(6a45)',v
          print,v
;x          readf,1,c
;x          print,c
;x          readf,1,format='(4a20)',v
;x          print,v
          readf,1,c
          print,c
          readf,1,lon
          print,lon
          readf,1,c
          print,c
          readf,1,lat
          print,lat
          readf,1,c
          print,c
          readf,1,pres
          readf,1,c
          print,c
          readf,1,t

          ps=fltarr(nlon,nlat)
          hgt=fltarr(nlon,nlat,nlev)
          temp=fltarr(nlon,nlat,nlev)
          rh=fltarr(nlon,nlat,nlev)
          uu=fltarr(nlon,nlat,nlev)
          vv=fltarr(nlon,nlat,nlev)

          for ifile=1,ktime   do begin 
           readf,1,c
           readf,1,time
           readf,1,c
           readf,1,ps
           readf,1,hgt
           readf,1,temp
           readf,1,rh
           readf,1,uu
           readf,1,vv
          endfor 

          close,1

        end

        pro ipt_ruc2,inputfile,instrument,nv,nlon,nlat,nlev,nt,v,lon,lat,$
                    pres,t,ktime,ps,hgt,temp,rh,uu,vv
          c=' '
        if(ktime eq 1)then begin
          openr,1,inputfile
          instrument=' '
          readf,1,format='(a20)',instrument
          print,instrument
          readf,1,c
          print,c
          readf,1,nv,nlon,nlat,nlev,nt
          print,nv,nlon,nlat,nlev,nt

          v=strarr(nv)
          lon=fltarr(nlon,nlat)
          lat=fltarr(nlon,nlat)
          pres=fltarr(nlev)
          t=fltarr(nt)

          readf,1,c
          print,c
;xie for twpice
          readf,1,format='(6a45)',v
          print,v
;          readf,1,format='(4a20)',v
;          print,v

          readf,1,c
          print,c
          readf,1,lon
;          print,lon
          readf,1,c
          print,c
          readf,1,lat
;          print,lat
          readf,1,c
          print,c
          readf,1,pres
          readf,1,c
          print,c
          readf,1,t
;stop
        endif
          ps=fltarr(nlon,nlat)
          hgt=fltarr(nlon,nlat,nlev)
          temp=fltarr(nlon,nlat,nlev)
          rh=fltarr(nlon,nlat,nlev)
          uu=fltarr(nlon,nlat,nlev)
          vv=fltarr(nlon,nlat,nlev)

;          for ifile=1,ktime   do begin 
           readf,1,c
           readf,1,time
           readf,1,c
           readf,1,ps
           readf,1,hgt
           readf,1,temp
           readf,1,rh
           readf,1,uu
           readf,1,vv
;          endfor 

          if(ktime eq nt)then begin
          close,1
          endif
        end
          

	pro opt_prof,outputfile,instrument,nv,np,nst,nt,v,stn,lat, $
           lon,alt,ht_low,ht_hi,t,d
	  openw,1,outputfile
	  instrument=' '
	  c=' '
	  printf,1,format='(a20)',instrument
	  print,instrument
          c=' nv  np  nst  nt'  
	  printf,1,c
	  print,c
	  printf,1,nv,np,nst,nt
	  print,nv,np,nst,nt

          c= 'v(nv)'
	  printf,1,c
	  print,c
	  printf,1,format='(4a20)',v
	  print,v

          c= ' stn(nst)'
	  printf,1,c
	  print,c
	  printf,1,stn
 
          c= ' lat(nst)'
	  printf,1,c
	  print,c
	  printf,1,lat

          c= 'lon(nst)'
	  printf,1,c
	  print,c
	  printf,1,lon

          c= ' alt(nst)'
	  printf,1,c
	  print,c
	  printf,1,alt

          c=' ht_low(np)'
	  printf,1,c
	  print,c
	  printf,1,ht_low

          c=' ht_hi(np)'
	  printf,1,c
	  print,c
	  printf,1,ht_hi

          c = 't(nt)'
	  printf,1,c
	  print,c
	  printf,1,t
	  print,'t(0),t(1),t(nt-1)'
	  print,t(0),t(1),t(nt-1)

          c = 'd(nv,np,nst,nt)'
	  printf,1,c
	  print,c
	  printf,1,d
	  close,1

	end

     pro INPUT_BLAYER,inputfile,nv,nterm,np,nt,v,p,t,d
      openr,1,inputfile
      c=' '
      readf,1,c
      readf,1,c
      readf,1,nv,nterm,np,nt
      v=strarr(nv,nterm)
      t=fltarr(nt)
      p=fltarr(np)
      d=fltarr(nv,nterm,np,nt)
      readf,1,c
      readf,1,format='(4a20)',v
      readf,1,c
      readf,1,p
      readf,1,c
      readf,1,t
      readf,1,c
      readf,1,d
      close,1
    end 
  
     pro INPUT_Bcolumn,inputfile,nv,nterm,nt,v,t,d
      openr,1,inputfile
      c=' '
      readf,1,c
      readf,1,c
      readf,1,nv,nterm,nt
      v=strarr(nv,nterm)
      t=fltarr(nt)
      d=fltarr(nv,nterm,nt)
      readf,1,c
      readf,1,format='(4a20)',v
      readf,1,c
      readf,1,t
      readf,1,c
      readf,1,d
      close,1
    end



   pro convert_ruc,ur,vr,u,v,x4,y4
    
    x1 = x4(0)-x4(2)
    y1 = y4(0)-y4(2)

    x2 = x4(1)-x4(3)
    y2 = y4(1)-y4(3)
    
    s1 = sqrt(x1^2+y1^2)
    s2 = sqrt(x2^2+y2^2)

    x1=x1/s1
    y1=y1/s1
    
    x2=x2/s2
    y2=y2/s2

    u = ur*x1+vr*x2
    v = ur*y1+vr*y2
  
 end
    



