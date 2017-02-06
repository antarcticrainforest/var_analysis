	pro itp_2point,t1,d1,t2,d2,missing_mark,t,d
            
          if(d1 gt missing_mark and d2 gt missing_mark) then begin
           if(t2 eq t1)then begin
             d=0.5*(d1+d2)
             goto,jump1
            endif
  	    d=d1+(d2-d1)/(t2-t1)*(t-t1)
	  endif
jump1:
	end

	pro itp_2p2d,t1,d1,t2,d2,t,d
            
           if(t2 eq t1)then begin
             d=0.5*(d1+d2)
             goto,jump1
            endif
  	    d=d1+(d2-d1)/(t2-t1)*(t-t1)
jump1:
	end


	pro itp_4point,t_2,d_2,t_1,d_1,t1,d1,t2,d2,missing_mark,t,d
	  itp_2point,t_1,d_1,t1,d1,missing_mark,t,d
	  if(d ne missing_mark) then goto,jump1

	  itp_2point,t_2,d_2,t1,d1,missing_mark,t,d 
	  if(d ne missing_mark) then goto,jump1

	  itp_2point,t_1,d_1,t2,d2,missing_mark,t,d

jump1:

	end



	pro interp,nt,t,d,missing_mark,nti,ti,di,nm,tm,dm
	  di=fltarr(nti)

	  tv=fltarr(nt)
	  dv=fltarr(nt)
	  lv=-1
	  for l=0,nt-1 do begin
	    if(d(l) gt missing_mark) then begin
	      lv=lv+1
	      if(t(l) gt missing_mark) then begin
	        tv(lv)=t(l)
	       endif else begin
	        tv(lv)=ti(l)
	      endelse
	      dv(lv)=d(l)
	     endif 
	  endfor
	  nv=lv+1
	  if(nv lt 1*nt/2) then begin
	    nm=nt-nv
	    tm=ti
	    dm=d
	    print,'nv < n*1/2 unable to interpolate'
            di(*)=-9999.0
	    goto,jump2
	  endif

	  lv0=0
	  for li=0,nti-1 do begin
	    for lv=lv0,nv-2 do begin
	      if(ti(li) ge tv(lv) and ti(li) lt tv(lv+1)) then begin	      
	        di(li)=dv(lv)+(dv(lv+1)-dv(lv))/(tv(lv+1)-tv(lv))$
                                               *(ti(li)-tv(lv))
		lv0=lv
	        goto,jump1
	      endif
	    endfor
	    if(ti(li) lt tv(0)) then begin
	      di(li)=dv(0)+(dv(1)-dv(0))/(tv(1)-tv(0))*(ti(li)-tv(0))
	    endif
	    if(ti(li) ge tv(nv-1)) then begin
	      di(li)=dv(nv-2)+(dv(nv-1)-dv(nv-2))/(tv(nv-1)-tv(nv-2))$
					         *(ti(li)-tv(nv-2))
	    endif
jump1:	  
	  endfor

	  tm=fltarr(nt)
	  dm=fltarr(nt)
	  lm=-1
	  for l=0,nt-1 do begin
	    if(d(l) le missing_mark) then begin
	      lm=lm+1
	      tm(lm)=ti(l)
	      dm(lm)=di(l)
	    endif
	  endfor
	  nm=lm+1

jump2: 

	end

	pro interpn,nt,t,d,missing_mark,nti,ti,di,nm,tm,dm
	  di=fltarr(nti)

          if(max(d) le missing_mark)then begin
           print, ' All data missing'
           di(*)=-9999.0
           goto,jump2
          endif

          tw=t
          if(min(t) le missing_mark)then begin
           tw(where(t le missing_mark))=ti(where(t le missing_mark))
          endif
 
          nv=n_elements(where(d gt missing_mark))
          dv=d(where(d gt missing_mark))
          tv=tw(where(d gt missing_mark))

	  if(nv lt 1*nt/2)then begin
	    nm=nt-nv
	    tm=ti
	    dm=d
	    print,'nv < n*1/2 unable to interpolate'
            di(*)=-9999.0
	    goto,jump2
	  endif

	  lv0=0
	  for li=0,nti-1 do begin
	    for lv=lv0,nv-2 do begin
	      if(ti(li) ge tv(lv) and ti(li) lt tv(lv+1)) then begin	      
	        di(li)=dv(lv)+(dv(lv+1)-dv(lv))/(tv(lv+1)-tv(lv))$
                                               *(ti(li)-tv(lv))
		lv0=lv
	        goto,jump1
	      endif
	    endfor
; not to extropolate
	    if(ti(li) lt tv(0)) then begin
;	      di(li)=dv(0)+(dv(1)-dv(0))/(tv(1)-tv(0))*(ti(li)-tv(0))
              di(li)=dv(0)
	    endif
	    if(ti(li) ge tv(nv-1)) then begin
;	      di(li)=dv(nv-2)+(dv(nv-1)-dv(nv-2))/(tv(nv-1)-tv(nv-2))$
;					         *(ti(li)-tv(nv-2))
              di(li)=dv(nv-1)
	    endif
jump1:	  
	  endfor

           nm=0
          if(min(d) le missing_mark) then begin
           nm=n_elements(where(d le missing_mark))
           tm=fltarr(nm)
           dm=tm
           tm(*)=ti(where(d le missing_mark))
           dm(*)=di(where(d le missing_mark))
          endif
; filled data for diagnostic purpose

jump2: 

	end




	pro interpg,nt,t,d,missing_mark,nti,ti,di,nm,tm,dm
	  di=fltarr(nti)

          if(max(d) le missing_mark)then begin
           di(*)=-9999.0
           print, ' All data missing'
           goto,jump2
          endif

          tw=t
          if(min(t) le missing_mark)then begin
           tw(where(t le missing_mark))=ti(where(t le missing_mark))
          endif
 
          nv=n_elements(where(d gt missing_mark))
          dv=d(where(d gt missing_mark))
          tv=tw(where(d gt missing_mark))

	  if(nv lt 5)then begin
	    nm=nt-nv
	    tm=ti
	    dm=d
            di(*)=-9999.0
	    print,'nv < 5 unable to interpolate'
	    goto,jump2
	  endif

	  lv0=0
	  for li=0,nti-1 do begin
	    for lv=lv0,nv-2 do begin
	      if(ti(li) ge tv(lv) and ti(li) lt tv(lv+1)) then begin	      
	        di(li)=dv(lv)+(dv(lv+1)-dv(lv))/(tv(lv+1)-tv(lv))$
                                               *(ti(li)-tv(lv))
		lv0=lv
	        goto,jump1
	      endif
	    endfor
; not to extropolate
	    if(ti(li) lt tv(0)) then begin
;	      di(li)=dv(0)+(dv(1)-dv(0))/(tv(1)-tv(0))*(ti(li)-tv(0))
              di(li)=dv(0)
	    endif
	    if(ti(li) ge tv(nv-1)) then begin
;	      di(li)=dv(nv-2)+(dv(nv-1)-dv(nv-2))/(tv(nv-1)-tv(nv-2))$
;					         *(ti(li)-tv(nv-2))
              di(li)=dv(nv-1)
	    endif
jump1:	  
	  endfor

           nm=0
          if(min(d) le missing_mark) then begin
           nm=n_elements(where(d le missing_mark))
           tm=fltarr(nm)
           dm=tm
           tm(*)=ti(where(d le missing_mark))
           dm(*)=di(where(d le missing_mark))
          endif
; filled data for diagnostic purpose

jump2: 

	end

	pro interuc,nt,t,d,missing_mark,nti,ti,di
	  di=fltarr(nti)

	  tv=fltarr(nt)
	  dv=fltarr(nt)
	  lv=-1
	  for l=0,nt-1 do begin
	    if(d(l) gt missing_mark) then begin
	      lv=lv+1
	      if(t(l) gt missing_mark) then begin
	        tv(lv)=t(l)
	       endif else begin
	        tv(lv)=ti(l)
	      endelse
	      dv(lv)=d(l)
	     endif 
	  endfor
	  nv=lv+1
	  if(nv lt 1*nt/2) then begin
	    nm=nt-nv
	    tm=ti
	    dm=d
            di(*)=-9999.0
	    print,'nv < n*1/2 unable to interpolate'
	    goto,jump2
	  endif

	  lv0=0
	  for li=0,nti-1 do begin
	    for lv=lv0,nv-2 do begin
	      if(ti(li) lt tv(lv) and ti(li) ge tv(lv+1)) then begin	      
	        di(li)=dv(lv)+(dv(lv+1)-dv(lv))/(tv(lv+1)-tv(lv))$
                                               *(ti(li)-tv(lv))
		lv0=lv
	        goto,jump1
	      endif
	    endfor
	    if(ti(li) ge tv(0)) then begin
;	      di(li)=dv(0)+(dv(1)-dv(0))/(tv(1)-tv(0))*(ti(li)-tv(0))
              di(li)=dv(0)
	    endif
	    if(ti(li) lt tv(nv-1)) then begin
;	      di(li)=dv(nv-2)+(dv(nv-1)-dv(nv-2))/(tv(nv-1)-tv(nv-2))$
;					         *(ti(li)-tv(nv-2))
              di(li)=dv(nv-1)
	    endif
jump1:	  
	  endfor

jump2:
	end
