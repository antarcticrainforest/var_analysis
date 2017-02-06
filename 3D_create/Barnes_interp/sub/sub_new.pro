	pro statistics,n,x,xa,sdx
	
	xa=0.0
	for i=0,n-1 do begin
	xa=xa+x(i)
	endfor
	xa=xa/n
	
	sdx=0.0
	for i=0,n-1 do begin
	sdx=sdx+(x(i)-xa)^2
	endfor
	sdx=(sdx/(n-1))^0.5
	
	end



        pro sdcheck,nt,d,missing_mark,multi
          d1=fltarr(nt)
          l1=-1
          for l=0,nt-1 do begin
            if(d(l) gt missing_mark) then begin
              l1=l1+1
              d1(l1)=d(l)
            endif
          endfor
          nt1=l1+1
	  if(nt1 gt 2) then begin
            statistics,nt1,d1,d1a,sdd1

            for l=0,nt-1 do begin
              if(d(l) gt missing_mark) then begin
                if(abs(d(l)-d1a) gt multi*sdd1) then begin
                  d(l)=missing_mark
                endif
	      endif
            endfor
	  endif

        end


	pro windowm,d,missing_mark,da,na
	  if(d gt missing_mark) then begin
	    da=da+d
	    na=na+1
	  endif
	end

	pro windown,n,x,dx,x1,l0,l1
	  for l=l0,n-1 do begin
	    xa=x(l)-dx*(0.5+0.1)
	    xb=x(l)+dx*(0.5-0.1)
;            print,l,l0
	    if(x1 ge xa and x1 lt xb) then begin
	      l1=l
;            print,l1,x(l),x1
	      goto,jump1
	    endif
	  endfor
	  l1=-1
jump1:	
	end

	pro windownx,n,x,dx,x1,l0,l1
	  for l=l0,n-1 do begin
;xie	    xa=x(l)-dx*(0.5-0.001)
	    xa=x(l)-dx*(0.5-0.001)
;xie	    xb=x(l)+dx*(0.5+0.001)
	    xb=x(l)+dx*(0.5-0.001)
;            print,l,l0
	    if(x1 ge xa and x1 lt xb) then begin
	      l1=l
;            print,l1,x(l),x1
	      goto,jump1
	    endif
	  endfor
	  l1=-1
jump1:	
	end


	pro new,nt,t,d,missing_mark,nt1,t1,dt1,d1
	  d1=fltarr(nt1)
	  na1=intarr(nt1)
	  l10=0
	  for l=0,nt-1 do begin
;	  for l=0,16 do begin
	    windown,nt1,t1,dt1,t(l),l10,l1
            ;   if(l eq 20)then begin
          ;       print,nt1,t1(l),dt1,t(l),l10,l1
            ;      stop
            ;    endif
	    if(l1 ge 0) then begin
	      xx=d1(l1)
	      ii=na1(l1)
	      windowm,d(l),missing_mark,xx,ii
	      d1(l1)=xx
	      na1(l1)=ii
	      l10=l1
	    endif
	  endfor	 	

	  for l1=0,nt1-1 do begin
          
	    if(na1(l1) gt 0) then begin
	      d1(l1)=d1(l1)/na1(l1)
	    endif else begin
	      d1(l1)=missing_mark
	    endelse
	  endfor
;xiestop
	end

	pro newx,nt,t,d,missing_mark,nt1,t1,dt1,d1
	  d1=fltarr(nt1)
	  na1=intarr(nt1)
	  l10=0
	  for l=0,nt-1 do begin
;	  for l=0,16 do begin
	    windownx,nt1,t1,dt1,t(l),l10,l1
            ;   if(l eq 20)then begin
          ;       print,nt1,t1(l),dt1,t(l),l10,l1
            ;      stop
            ;    endif
	    if(l1 ge 0) then begin
	      xx=d1(l1)
	      ii=na1(l1)
	      windowm,d(l),missing_mark,xx,ii
	      d1(l1)=xx
	      na1(l1)=ii
	      l10=l1
	    endif
	  endfor	 	

	  for l1=0,nt1-1 do begin
          
	    if(na1(l1) gt 0) then begin
	      d1(l1)=d1(l1)/na1(l1)
	    endif else begin
	      d1(l1)=missing_mark
	    endelse
	  endfor
;xiestop
	end

      pro cal_ins,t,lon,lat,ins,cosz

      tw=2.0*3.1416*(t-1.)/365.0
      ecc= 1.000110+0.034221*COS(tw)+0.001280*SIN(tw)+   $
             0.000719*COS(2.0*tw)+0.000077*SIN(2.0*tw) ; eccentricity

      pif=3.1416/180.0

      delta=.006918 - .399912*cos(tw) + .070257*sin(tw) -    $
            .006758*cos(2.*tw) + .000907*sin(2.*tw) -        $
            .002697*cos(3.*tw) + .001480*sin(3.*tw)       

      cr1=pif*279.367 + 0.985647*365.0/360.0*tw

      dt=-105.4*SIN(cr1)+596.2*SIN(2*cr1)+4.3*SIN(3*cr1)-12.7*SIN(4*cr1)- $
         429.2*COS(cr1)-2.1*COS(2*cr1)+19.3*COS(3*cr1)  ; in seconds

      tt=dt/3600.0/24.0*2.0*3.1416/365.0+tw;

      cosz=sin(lat*pif)*sin(delta) - cos(lat*pif)*cos(delta)*   $
            cos(tt*365.0+lon*pif)

      ins=1368.2*cosz*ecc
;      ins=1378.95*cosz*ecc

      if(min(ins) lt 0.0)then begin
      ins(where(ins lt 0.0))=0.0
      endif
end

      pro checkloc,x2,y2,xb,yb,nb,ramp,nc
      clon=xb(nb-1)
      clat=yb(nb-1)
       
       x=fltarr(nb-1)  & y=x
       x =xb(0:nb-2)-clon
       y =yb(0:nb-2)-clat
       x=x*ramp
       y=y*ramp
      
       theta2,w,x,y

       xc=x2-clon
       yc=y2-clat
       theta1,angle,xc,yc

       n=nb-1
        for i=1,n-1 do begin
         if(angle le w(i) and angle gt w(i-1))then begin
          i1=i-1
          i2=i
          goto,jump10
         endif   
        endfor
       
        if(angle le w(0) and angle gt w(n-1))then begin
         i1=n-1
         i2=0
         goto,jump10
        endif

         if(angle le min(w) or angle gt max(w))then begin
         i1=where(w eq max(w))
         i2=where(w eq min(w))
         endif
jump10:

; cc  the four points are x(i1),y(i1),x(i2),y(i2)

      ck=(y(i1)-y(i2))/(x(i1)-x(i2))
      if(abs(xc) le 1.0e-3)then begin
       ybound=y(i1)-ck*x(i1)
       xbound=0.0
       endif
      if(abs(xc) gt 1.0e-3)then begin
       xbound=(ck*x(i1)-y(i1))/(ck-yc/xc)
       ybound=yc/xc*xbound
      endif

       rc=xc*xc+yc*yc
       rbound=xbound*xbound+ybound*ybound

       nc=0
       if(rc le rbound(0))then begin
       nc=1
       endif
 end

       pro theta1,w,x,y
        c=sqrt(x*x+y*y)
        
         if(c le 1.0e-5)then begin
          w=0.0  
         endif
        w=acos(x/c)*180.0/3.1416
        if(y lt 0)then begin
          w=360.0-w
        endif

    end

       pro theta2,w,x,y
        c=sqrt(x*x+y*y)
       
         if( min(c) le 1.0e-5)then begin
         c(where (c le 1.0e-5) )=1.0
         endif

        w=acos(x/c)*180.0/3.1416

        if(min(y) lt 0.0)then begin
         w(where(y lt 0.0))=360.0-w(where(y lt 0.0))
        endif       
 end

        pro finddmin,xlon1,xlat1,xlon2,xlat2,i1,i2
        ddd1=xlon1 
        n=n_elements(ddd1)
        ind2=intarr(n)

        for i=0,n-1 do begin
         cdist,xlon2,xlat2,xlon1(i),xlat1(i),dd
         ddd1(i)=min(dd)
         ik2=where(dd eq min(dd))
         ind2(i)=ik2(0)
        endfor

         ik1=where(ddd1 eq min(ddd1))
         i1=ik1(0)

         ik3=ind2(where(ddd1 eq min(ddd1)))
         i2=ik3(0)
end
        
         pro gmean,mg,dp,nv,nst,nt,tsmooth=tsmooth
 
          mg=fltarr(nt)
          ww=fltarr(nst)
           for i=0,nt-1 do begin
           ww(*)=dp(nv-1,*,i)
           mg(i)=total(ww)/nst
           endfor
        if(keyword_set(tsmooth))then begin
          mg1=mg
          mg=smooth(mg1,tsmooth)
         endif
   end 



  pro vertint,p,np,ps,nt,d,dw,fac
   dw=fltarr(nt)
   for i=0,nt-1 do begin
     for ilev=1,np-1 do begin
      dw(i)=dw(i)+d(0,ilev,i)*50.0
     endfor
     if(ps(i) ge p(0))then begin
      dw(i)=dw(i)+d(0,0,i)*(ps(i)-(p(0)-25.0))
     endif
     if(ps(i) lt p(0))then begin
      dw(i)=dw(i)+d(0,1,i)*(ps(i)-(p(0)-25.0))
     endif
   endfor
   dw=dw*fac
  end








