;------------------------------
pro get_matrix,dobs,dtru,m1,m2

 nst=n_elements(dobs(*,0))
 nsta=n_elements(dtru(*,0))
 nt = n_elements(dobs(0,*))
 
 m1=fltarr(nst,nst)-100.
 m2=fltarr(nst,nsta)-100.

 for i=0,nst-1 do begin
   data1=reform(dobs(i,*))
   jj=where(data1 ge -200.0, count)

  if(count ge nt/3)then begin

   for j=0,nst-1 do begin
     data2=reform(dobs(j,*))
     kk = where((data2 ge -200.) and (data1 ge -200.), count2)
     if(count2 ge nt/3)then begin
      m1(i,j)= c_correlate(data1(kk),data2(kk),0)
     endif 
   endfor


   for j=0,nsta-1 do begin
     data2=reform(dtru(j,*))
     m2(i,j)=c_correlate(data1(jj),data2(jj),0)
   endfor

  endif 

 endfor

end

; -------------------------

function rebin3,data1
 data2=fltarr(143)
 for i=0,142 do begin
  i1=i*3+1
  i2=i1+2
  jj=where(data1(i1:i2) ge -200., count)
  if(count eq 0)then begin
   data2(i)=-9999.0
  endif else begin
   data2(i)=total(data1(i1+jj))/n_elements(jj)
  endelse
 endfor
 return,data2
 end




; ---------------------------
pro over_sond,ds,k1,it
 ; station data out
   for i=0,4 do begin
    x0=ds(2,k1,i,it)
    y0=ds(3,k1,i,it)
    u0=ds(4,k1,i,it)
    v0=ds(5,k1,i,it)
    if(min([x0,y0,u0,v0]) gt -200.)then begin
      u0=strdigit(u0,1)
      v0=strdigit(v0,1)
      xyouts,x0,y0,'* '+ u0,align=0.0,charsize=0.5,color=2
;                        !!! 
    endif
   endfor
end
; --------------------------------------
 pro over_prof,lonp,latp,dp,k1,itpr

   for i=0,16 do begin
    x0=lonp(i)
    y0=latp(i)
    u0=dp(2,k1,i,itpr)
    v0=dp(3,k1,i,itpr)
    if(min([x0,y0,u0,v0]) gt -200.)then begin
      u0=strdigit(u0,1)
      v0=strdigit(v0,1)
      xyouts,x0,y0,'o '+ u0,align=0.0,charsize=0.4,color=1
;                        !!!
    endif else begin
      xyouts,x0,y0,'o'+' XX',align=0.0,charsize=0.4,color=1
    endelse
   endfor
 end


; -----------------------------
 pro over_data,lonp,latp,dp,cut=cut,shifty=shifty,size=size
    n=n_elements(lonp)
 
 if(not keyword_set(size))then size=1.
   
   dy=0
   symb='X'
   ;symb=' '
   if(keyword_set(shifty))then begin
       dy=shifty
       symb='  '
   endif

    for i=0,n-1 do begin
       x0=lonp(i)
       y0=latp(i)
       u0=dp(i)

       jcut = 1
       icut = 1

   if(keyword_Set(cut))then begin
       jcut = ( (x0 le -94.) and (x0 ge -100.))
       icut = ((y0 le 40.) and (y0 ge 34.0) )
   endif
       y0=y0-dy

   if(jcut*icut eq 1)then begin
       if((u0 ge -400) )then  begin
          u0=strdigit(u0,1)
          xyouts,x0,y0,symb+ u0,align=0.0,charsize=size
       endif else begin
          xyouts,x0,y0,symb,align=0.0
       endelse
   endif
     endfor
 end
; -------------------

function strdigit,a,n
 b=long(a*10^n)
 b=strtrim(b,2)
 len1=strlen(b)
 c=strmid(b,0,len1-n)+'.'+strmid(b,len1-n,n)
 return,c
end

function rebin2,a,n1,n2
 b=fltarr(n1,n2)
 for j=0,n2-1 do begin
  k1=j*n1
  k2=k1+n1-1
  b(*,j)=a(k1:k2) 
 endfor
 return,b
end


;==================================================================
  pro stati_int,nsto,nstan,nt,dobs,d_true,a_ana,a_obs, $
               nstob,jjobs,matr1,matr2,percent=percent

;==================================================================
; analysis value will be equal to -9999.0 on a_obs if length < nt/3

 if(not keyword_set(percent))then percent=100.0   ; precentage eigen

 a_ana=fltarr(nstan,nt)-9999.0
 a_obs=fltarr(nsto, nt)-9999.0

;1 extract valid data
 jjobs=-1
  for i=0,nsto-1 do begin
     dw=reform(dobs(i,*))
     j1=where(dw ge -500,count)
     if(count gt nt/3)then jjobs=[jjobs,i]
  endfor

  
  nstob=n_elements(jjobs)
  if(nstob le 2)then begin
     print,'there is only one valid measurement'
     a_ana = d_true
     a_obs = dobs
     return
  endif
  jjobs=jjobs(1:*)
  nstob=nstob-1

  d_obs=fltarr(nstob,nt)
  for i=0,nstob-1 do begin
    j=jjobs(i)
    d_obs(i,*)=dobs(j,*)
  endfor

; d_obs(nstob,nt) ......... as true observations

;2 get correlation coefficients
     
;-------------------------------------------
   get_matrix,d_obs,d_true,matr1,matr2   
;                      (nstob,nstob),(nstob,nstan) 
;--------------------------------------------
 
;3 starting the fitting process

  
;;; for it=100,50,-49 do begin
;;; print,'it',it,a_ana(*,it)

 for it=0,nt-1 do begin
;;  print,it
; ------------------------------------------------------
;3.1 take valid data
  
  fcar = reform(d_obs(*,it))       ; fixed time

  jj=where(fcar gt -200.,count)

  if(count le 1)then begin 
    a_ana(*,it)=d_true(*,it)
    a_obs(*,it)=dobs(*,it)
    goto,jump10
  endif

  fcar=fcar(jj)                   ; reduced data and reduced matrix

; filter the data

  cv = matr1(jj,*)
  cv = cv(*,jj)

  
 sw=cv
 trired,sw,dk,ek
 triql,dk,ek,sw

 kk=reverse(sort(dk))         ; big to small
 dk=dk(kk)
 nn=n_elements(kk)
 sw2=sw
 for i=0,nn-1 do sw(*,i)=sw2(*,kk(i))  ; eigenvalues same order
 tvalue=0.0
 tvalue1=total(dk(where(dk gt 0.001)))
 i1=nn-1
 for i=0,nn-1 do begin 
  tvalue=tvalue+dk(i)/tvalue1*100.
  if(tvalue ge percent)then begin
    i1=i 
    goto,jumpout1
  endif
 endfor

 i1= (i1 > nn/3)  ; put a limit on it
jumpout1:

 ; i1 eigenvalues
 ; projection
  cw=transpose(sw)#fcar
  if(i1 lt (nn-1))then cw(i1+1:*)=0.0   ; set coefficients to zero

  fcar = reform(cw#transpose(sw))      ; set back, filtered data

; ----------------------------------------
; algorithms

 cv2=invert(cv)

 for ist=0,nstan-1 do begin
  bv = reform(matr2(jj,ist)) 
  wv = cv2#bv
  ffit = total(wv*fcar)
  a_ana(ist,it) = ffit
 endfor

 for ist=0,nstob-1 do begin
  bv = reform(matr1(jj,ist)) 
  wv = cv2#bv
  ffit = total(wv*fcar)
  a_obs(jjobs(ist),it) = ffit
 endfor

jump10:
 ;print, '2----'
  ;print,a_ana(*,it)
endfor
; +++++++++++++++++++++++++ for it

end

;-----------------------------

 pro calc_mean,d4,dmean,subt=subt
; subt = 0, calculate and remove
; 1 add back
; -1 subtract

  nv  =n_elements(d4(*,0,0,0))
  np =n_elements(d4(0,*,0,0))
  nst=n_elements(d4(0,0,*,0))
  nt =n_elements(d4(0,0,0,*))


  d5=d4

  if(keyword_Set(subt))then begin  ; add the means back
   nsam = n_elements(dmean(0,0,*))
   for m=0,nv-1 do begin
   for k=0,np-1 do begin
   for i=0,nt-1 do begin
    i1 = (i mod nsam)
    d4(m,k,*,i)=d5(m,k,*,i) + subt*dmean(m,k,i1)
   endfor
   endfor
   endfor

  jj=where(d5 le -2000., count)
  if(count ge 1) then d4(jj)=-9999.0
  return
  endif

  nsamp=8
  dmean=fltarr(nv,np,nsamp)
   for m=0,nv-1 do begin
   for k=0,np-1 do begin
   for i=0,nsamp-1 do begin
    jj1 = indgen(nt)*nsamp+i
    jj1=jj1(where(jj1 lt nt))
    dw = reform(d5(m,k,*,jj1))
    jj2=where(dw ge -2000.,count)
    if(count ge 1)then begin
      dmean(m,k,i)=total(dw(jj2))/n_elements(jj2)
    endif else begin
      dmean(m,k,i)=-9999.0
    endelse
   endfor
   endfor
   endfor
 
   calc_mean,d4,dmean,subt=-1
 end
;-----------------------------

 pro calc_std,d4,dstd,subt=subt
; subt = 0, calculate and remove
; 1 add back
; -1 normalize

  nv  =n_elements(d4(*,0,0,0))
  np =n_elements(d4(0,*,0,0))
  nst=n_elements(d4(0,0,*,0))
  nt =n_elements(d4(0,0,0,*))


  d5=d4

  if(keyword_Set(subt))then begin  ; add the means back

 if(subt eq 1)then begin
   for m=0,nv-1 do begin
   for k=0,np-1 do begin
    d4(m,k,*,*)=d5(m,k,*,*)*dstd(m,k)
   endfor
   endfor
 endif


 if(subt eq -1)then begin
   for m=0,nv-1 do begin
   for k=0,np-1 do begin
    d4(m,k,*,*)=d5(m,k,*,*)/dstd(m,k)
   endfor
   endfor
 endif

  jj=where(d5 le -2000.,count)
  if(count ge 1)then d4(jj)=-9999.0
  return
  endif

  dstd=fltarr(nv,np)
   for m=0,nv-1 do begin
   for k=0,np-1 do begin
    dw = reform(d5(m,k,*,*))
    jj2=where(dw ge -2000.,count)
    if(count ge 2)then begin
      dw=dw(jj2)
      dstd(m,k)=total(dw*dw)
      dstd(m,k)=sqrt(dstd(m,k)/n_elements(jj2))
    endif else begin
      dstd(m,k)=1.
    endelse
   endfor
   endfor
   jj=where(dstd le 0.0001,count)
   if(count gt 0)then dstd(jj)=0.0001

 
   calc_std,d4,dstd,subt=-1
 end


; ------------------------------------------
  pro calc_sm,nt2,np2,d4
  
  if((nt2 le 0) and (nt2 le 0))then return

  nv  =n_elements(d4(*,0,0,0))
  np =n_elements(d4(0,*,0,0))
  nst=n_elements(d4(0,0,*,0))
  nt =n_elements(d4(0,0,0,*))

  d5=d4


  if(nt2 gt 0)then begin

   for m=0,nv-1 do begin
   for k=0,np-1 do begin
   for ist=0,nst-1 do begin
   for i=0,nt-1 do begin
     it1=((i-nt2)>0)
     it2=((i+nt2)<(nt-1))
     dw=d5(m,k,ist,it1:it2)
     jj=where(dw ge -2000,count)
     if(count gt 0)then begin
       d4(m,k,ist,i)=total(dw(jj))/n_elements(jj)
     endif
   endfor
   endfor
   endfor
   endfor
 endif

d5=d4

  if(np2 gt 0)then begin

   for m=0,nv-1 do begin
   for k=0,np-1 do begin
   for ist=0,nst-1 do begin
   for i=0,nt-1 do begin
     ip1=((k-np2)>0)
     ip2=((k+np2)<(np-1))
     dw=d5(m,ip1:ip2,ist,i)
     jj=where(dw ge -2000,count)
     if(count gt 0)then begin
       d4(m,k,ist,i)=total(dw(jj))/n_elements(jj)
     endif
   endfor
   endfor
   endfor
   endfor
 endif

end

