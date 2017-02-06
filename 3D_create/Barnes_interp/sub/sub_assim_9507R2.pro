        pro itps,ks,kb,d,ds
	  if(ks lt kb) then begin
            d(ks)=ds
            for k=ks+1,kb-1 do begin
              d(k)=ds+(d(kb)-ds)/(kb-ks)*(k-ks)
            endfor
	  endif
        end


	pro diverg_2d,unith,var,u,v,divu,divv,div1
	  uvar=u*var
	  vvar=v*var
	  div1=total( (uvar*divu+vvar*divv) )
	end
	
	pro diverg,unith,var,u,v,divu,divv,div1
	  uvar=u*var
	  vvar=v*var
	  div1=(uvar*divu+vvar*divv)#unith
	end
	
	
	pro fcorlx,unith,nstu,f,v,fcx1
	  fcx1=(v*f/nstu)#unith
	end
	
	
	pro fcorly,unith,nstu,f,u,fcy1
	  fcy1=(-u*f/nstu)#unith
	end
	
	
	pro fpgd,unith,z,dzdx,fp1
	  g=9.8
	  fp1=(-g*z*dzdx)#unith
	end
	
	
        pro assim,np,dp,p,ks,kb,kt,nstu,lonu,latu,du,nsts,lons,lats,dss,$
                  divbvar,fcx,fcy,fpx,fpy,budget,nad,nadvar,wvar,dus
	  f=fltarr(np,nstu)
	  dzdx=fltarr(np,nstu)
	  dzdy=fltarr(np,nstu)
	  divu=fltarr(np,nstu)
	  divv=fltarr(np,nstu)
	  for k=kb,kt do begin
	    lonu1=du(8,k,*)
	    latu1=du(9,k,*)
            horizontal_field,nstu,lonu1,latu1,x,y,f1,dzdx1,dzdy1,divu1,divv1
	    f(k,*)=f1
	    dzdx(k,*)=dzdx1
	    dzdy(k,*)=dzdy1
	    divu(k,*)=divu1
	    divv(k,*)=divv1
	  endfor
          horizontal_field,nsts,lons,lats,xs,ys,fs,dzdxs,dzdys,divus,divvs

;print ,'d1'

	  Rd=287.04
          g=9.8

          nvbudget_column=5
          nvbudget_layer=6

          ad=[1.0,1.0,1.0,1.0,1.0]
	  for m=0,4 do begin
            wvar(m,*,*)=wvar(m,*,*)/ad(m)
	  endfor

          for m=0,nvbudget_column-1 do begin
            for k=0,np-1 do begin
              for ist=0,nstu-1 do begin
                du(m,k,ist)=du(m,k,ist)*ad(m)
              endfor
            endfor
            for ist=0,nsts-1 do begin
              dss(m,ist)=dss(m,ist)*ad(m)
            endfor
            nterm=budget(m,0)
            for iterm=1,nterm+1 do begin
              budget(m,iterm)=budget(m,iterm)*ad(m)
            endfor
          endfor
;print ,'d2'

	  unith=fltarr(nstu)
	  uniths=fltarr(nsts)
	  unitv=fltarr(np)
	  unitv1=fltarr(np)
	
	  c=fltarr(nvbudget_column)
	  b=fltarr(nvbudget_column)
	  dbdvar=fltarr(nvbudget_column,nvbudget_column,np,nstu)
	  a=fltarr(nvbudget_column,nvbudget_column)
	  ld=fltarr(nvbudget_column)
	
	  bvar=fltarr(np,nstu)
	  unit=fltarr(np,nstu)
	  u=fltarr(np,nstu)
	  v=fltarr(np,nstu)
	  r=fltarr(np,nstu)
	  h=fltarr(np,nstu)
	  z=fltarr(np,nstu)
	  T=fltarr(np,nstu)
	  divbvar=fltarr(nvbudget_layer,np)

	  bvar1=fltarr(np,nstu)
	  u1=fltarr(np,nstu)
	  v1=fltarr(np,nstu)
	  r2=fltarr(np)
	  h2=fltarr(np)
	  z2=fltarr(np)
	  z1=fltarr(np,nstu)

	  bvars=fltarr(1,nsts)
	  us=fltarr(1,nsts)
	  vs=fltarr(1,nsts)
	  zs=fltarr(1,nsts)
	  Ts=fltarr(1,nsts)

	  madn=[3,4,1,2]
	  
	  a1=fltarr(nad,nad)
	  b1=fltarr(nad)

    	  dus=du
	
	  unith(*)=1.0
	  uniths(*)=1.0
          unitv(kb:kt)=1.0
	  unitv1(ks:kt)=1.0
;recover
;print ,'d3'
          unitv1(ks)=0.5+(budget(0,6)-p(ks))/dp*100.
          unitv1(kt)=0.5
          unitv(kb)=0.5+(budget(0,6)-p(kb))/dp*100.
          unitv(kt)=0.5
;recover hybrid
	
	  for mb=0,nvbudget_column-1 do begin
	    c(mb)=0
	    nterm=budget(mb,0)
	    for item=2,nterm do begin
	      c(mb)=c(mb)-budget(mb,item)
	    endfor
	  endfor
	  
	  for ist=0,nstu-1 do begin
	    for k=kb,kt do begin
	      unit(k,ist)=dus(0,k,ist)
	      r(k,ist)=dus(1,k,ist)
	      h(k,ist)=dus(2,k,ist)
	      u(k,ist)=dus(3,k,ist)
	      v(k,ist)=dus(4,k,ist)
	      z(k,ist)=dus(6,k,ist)
	      T(k,ist)=dus(5,k,ist)
	    endfor
	  endfor
	  
	  for ist=0,nsts-1 do begin
	    us(0,ist)=dss(3,ist)
	    vs(0,ist)=dss(4,ist)
	    zs(0,ist)=dss(6,ist)
	    Ts(0,ist)=dss(5,ist)
	  endfor
;print ,'d4'
	  
	  for ist=0,nstu-1 do begin
	    for k=kb,kt do begin
	      dbdvar(0,0,k,ist)=unit(k,ist)*divu(k,ist)*dp/g
	      dbdvar(0,1,k,ist)=unit(k,ist)*divv(k,ist)*dp/g
	      dbdvar(1,0,k,ist)=r(k,ist)*divu(k,ist)*dp/g
	      dbdvar(1,1,k,ist)=r(k,ist)*divv(k,ist)*dp/g
	      dbdvar(1,2,k,ist)=(u(k,ist)*divu(k,ist)+v(k,ist)*divv(k,ist))*dp/g
	      dbdvar(2,0,k,ist)=h(k,ist)*divu(k,ist)*dp/g
	      dbdvar(2,1,k,ist)=h(k,ist)*divv(k,ist)*dp/g
	      dbdvar(2,3,k,ist)=(u(k,ist)*divu(k,ist)+v(k,ist)*divv(k,ist))*dp/g
	      dbdvar(3,0,k,ist)=(2.0*u(k,ist)*divu(k,ist)+v(k,ist)*divv(k,ist))*dp/g
 	      dbdvar(3,1,k,ist)=(u(k,ist)*divv(k,ist)-f(k,ist)/nstu)*dp/g
              dbdvar(3,3,k,ist)=(kt+1-k)*Rd*dp/100.0/p(k)*dzdx(k,ist)*dp/g
	      dbdvar(4,0,k,ist)=(v(k,ist)*divu(k,ist)+f(k,ist)/nstu)*dp/g
	      dbdvar(4,1,k,ist)=(u(k,ist)*divu(k,ist)+2.0*v(k,ist)*divv(k,ist))*dp/g
              dbdvar(4,3,k,ist)=(kt+1-k)*Rd*dp/100.0/p(k)*dzdy(k,ist)*dp/g
	    endfor
	  endfor
	  
;print ,'d5'
	  for mb=0,4 do begin
	    for ist=0,nstu-1 do begin
	      for k=kb,kt do begin
	        bvar(k,ist)=dus(mb,k,ist)
	      endfor
	    endfor
	    
	    for ist=0,nsts-1 do begin
	      bvars(0,ist)=dss(mb,ist)
	    endfor

	    for mb1=0,4 do begin
	      for ist=0,nstu-1 do begin
	        for k=kb,kt do begin
	          u1(k,ist)=-dbdvar(mb1,0,k,ist)/2.0/wvar(3,k,ist)
	          v1(k,ist)=-dbdvar(mb1,1,k,ist)/2.0/wvar(4,k,ist)
                      r2(k)=0.0
                      h2(k)=-dbdvar(mb1,3,k,ist)/2.0/wvar(2,k,ist)
	        endfor
		height,np,kb,kt,p,h2,r2,z2
                z1(*,ist)=z2(*)
	      endfor
	      diverg,unith,bvar,u1,v1,divu,divv,div2
	      sdiv2=transpose(div2)#unitv*dp/g
	      a(mb,mb1)=sdiv2(0)
	      if(abs(a(mb,mb1)) gt 100) then begin
	        print,'!!!!!!!!!!',mb,mb1,a(mb,mb1)
;	        stop
	      endif
	      if(mb eq 3) then begin
	        fcorlx,unith,nstu,f,v1,fcx1
	        sfcx1=transpose(fcx1)#unitv*dp/g
                fpgd,unith,z1,dzdx,fpx1
                sfpx1=transpose(fpx1)#unitv*dp/g
                a(3,mb1)=a(3,mb1)-sfcx1(0)-sfpx1(0)
	      endif
	      if(mb eq 4) then begin
	        fcorly,unith,nstu,f,u1,fcy1
	        sfcy1=transpose(fcy1)#unitv*dp/g
                fpgd,unith,z1,dzdy,fpy1
                sfpy1=transpose(fpy1)#unitv*dp/g
                a(4,mb1)=a(4,mb1)-sfcy1(0)-sfpy1(0)
	      endif
	    endfor
	    
	    diverg,unith,bvar,u,v,divu,divv,div1
	    diverg,uniths,bvars,us,vs,divus,divvs,divs
	    itps,ks,kb,div1,divs(0)
	    for k=ks,kt do begin
	      divbvar(mb,k)=div1(k)/ad(mb)
	    endfor
	    sdiv1=transpose(div1)#unitv1*dp/g
	    b(mb)=sdiv1(0)+c(mb)
	    nterm=budget(mb,0)
	    budget(mb,nterm+1)=-sdiv1(0)
	  endfor

;print ,'d6'
	  diverg,unith,T,u,v,divu,divv,div1
	  diverg,uniths,Ts,us,vs,divus,divvs,divs
	  itps,ks,kb,div1,divs(0)
          for k=ks,kt do begin
            divbvar(5,k)=div1(k)
          endfor
	  	  
          for mvar=0,nadvar-1 do begin
	    m=madn(mvar)
            for mb1=0,4 do begin
	      for ist=0,nstu-1 do begin
	        for k=kb,kt do begin
	          bvar1(k,ist)=-dbdvar(mb1,mvar,k,ist)/2.0/wvar(m,k,ist)
		endfor
              endfor
	      diverg,unith,bvar1,u,v,divu,divv,div1
	      sdiv1=transpose(div1)#unitv*dp/g
	      a(m,mb1)=a(m,mb1)+sdiv1(0)
	    endfor
	  endfor

;print ,'d7'
	  fcorlx,unith,nstu,f,v,fcx1
;print ,'d71'
	  fcorlx,uniths,nsts,fs,vs,fcxs
	  itps,ks,kb,fcx1,fcxs(0)
;print ,'d72'
	  sfcx1=transpose(fcx1)#unitv1*dp/g
	  fpgd,unith,z,dzdx,fpx1
	  fpgd,uniths,zs,dzdxs,fpxs
;print ,'d73'
	  itps,ks,kb,fpx1,fpxs(0)
	  sfpx1=transpose(fpx1)#unitv1*dp/g
	  b(3)=b(3)-sfcx1(0)-sfpx1(0)
	  fcorly,unith,nstu,f,u,fcy1
	  fcorly,uniths,nsts,fs,us,fcys
;print ,'d74'
	  itps,ks,kb,fcy1,fcys(0)
	  sfcy1=transpose(fcy1)#unitv1*dp/g
	  fpgd,unith,z,dzdy,fpy1
	  fpgd,uniths,zs,dzdys,fpys
	  itps,ks,kb,fpy1,fpys(0)
	  sfpy1=transpose(fpy1)#unitv1*dp/g
;print ,'d75'
	  b(4)=b(4)-sfcy1(0)-sfpy1(0)
	  budget(3,5)=sfcx1(0)
	  budget(3,6)=sfpx1(0)
	  budget(4,5)=sfcy1(0)
	  budget(4,6)=sfpy1(0)
	  fcx=fcx1
	  fcy=fcy1
	  fpx=fpx1
	  fpy=fpy1
	  for mb=0,4 do begin
	    budget(mb,1)=-b(mb)
	  endfor
	  b=-b
	  
;print ,'d76'
	  a1(*)=0.0
	  b1(*)=0.0
	  for i1=0,nad-1 do begin
	    for i2=0,nad-1 do begin
	      a1(i1,i2)=a(i1,i2)
	    endfor
	    b1(i1)=b(i1)
	  endfor
;;	  print,'a1'
;;	  print,a1
	  a2=a1
;;	  print,'b1'
;;	  print,b1
	  
;;	  print,''
;print ,'d77'
  ;stop
	  if(nad lt 2) then begin
	    ld1=fltarr(nad)
	    ld1(0)=b1(0)/a1(0,0)
;;	    print,'ld1'
;;	    print,ld1
;;	    print,'b2'
	    print,'b2',a2#ld1
	   endif else begin
	    nr_ludcmp,a1,index
            ld1=nr_lubksb(a1,index,b1)
;;	    print,'ld1'
;;	    print,ld1
;;	    print,'b2'
	    print,'b2',a2#ld1
	  endelse
	  
	  for i=0,nad-1 do begin
	    ld(i)=ld1(i)
	  endfor
;print ,'d8'
	  
	  for ist=0,nstu-1 do begin
	    for k=kb,kt do begin
	      for mad=0,nadvar-1 do begin
	        m=madn(mad)
	        for mb=0,nad-1 do begin
	          dus(m,k,ist)=dus(m,k,ist)-ld(mb)*dbdvar(mb,mad,k,ist)$
                                                   /2.0/wvar(m,k,ist)
	        endfor
	      endfor
	    endfor
	  endfor

          for m=0,nvbudget_column-1 do begin
            for k=0,np-1 do begin
              for ist=0,nstu-1 do begin
                dus(m,k,ist)=dus(m,k,ist)/ad(m)
              endfor
            endfor
            nterm=budget(m,0)
            for iterm=1,nterm+1 do begin
              budget(m,iterm)=budget(m,iterm)/ad(m)
            endfor
          endfor
;print,'d9'
	end

        pro assim2,np,dp,p,ks,kb,kt,nstu,lonu,latu,du,nsts,lons,lats,dss,$
                  divbvar,fcx,fcy,fpx,fpy,budget,nad,nadvar,wvar,dus
	  f=fltarr(np,nstu)
	  dzdx=fltarr(np,nstu)
	  dzdy=fltarr(np,nstu)
	  divu=fltarr(np,nstu)
	  divv=fltarr(np,nstu)
	  for k=kb,kt do begin
	    lonu1=du(8,k,*)
	    latu1=du(9,k,*)
            horizontal_field,nstu,lonu1,latu1,x,y,f1,dzdx1,dzdy1,divu1,divv1
	    f(k,*)=f1
	    dzdx(k,*)=dzdx1
	    dzdy(k,*)=dzdy1
	    divu(k,*)=divu1
	    divv(k,*)=divv1
	  endfor
          horizontal_field,nsts,lons,lats,xs,ys,fs,dzdxs,dzdys,divus,divvs

	  Rd=287.04
          g=9.8

          nvbudget_column=5
          nvbudget_layer=6

          ad=[1.0,1.0,1.0,1.0,1.0]
	  for m=0,4 do begin
            wvar(m,*,*)=wvar(m,*,*)/ad(m)
	  endfor

          for m=0,nvbudget_column-1 do begin
            for k=0,np-1 do begin
              for ist=0,nstu-1 do begin
                du(m,k,ist)=du(m,k,ist)*ad(m)
              endfor
            endfor
            for ist=0,nsts-1 do begin
              dss(m,ist)=dss(m,ist)*ad(m)
            endfor
            nterm=budget(m,0)
            for iterm=1,nterm+1 do begin
              budget(m,iterm)=budget(m,iterm)*ad(m)
            endfor
          endfor

	  unith=fltarr(nstu)
	  uniths=fltarr(nsts)
	  unitv=fltarr(np)
	  unitv1=fltarr(np)
	
	  c=fltarr(nvbudget_column)
	  b=fltarr(nvbudget_column)
	  dbdvar=fltarr(nvbudget_column,nvbudget_column,np,nstu)
	  a=fltarr(nvbudget_column,nvbudget_column)
	  ld=fltarr(nvbudget_column)
	
	  bvar=fltarr(np,nstu)
	  unit=fltarr(np,nstu)
	  u=fltarr(np,nstu)
	  v=fltarr(np,nstu)
	  r=fltarr(np,nstu)
	  h=fltarr(np,nstu)
	  z=fltarr(np,nstu)
	  T=fltarr(np,nstu)
	  divbvar=fltarr(nvbudget_layer,np)

	  bvar1=fltarr(np,nstu)
	  u1=fltarr(np,nstu)
	  v1=fltarr(np,nstu)
	  r2=fltarr(np)
	  h2=fltarr(np)
	  z2=fltarr(np)
	  z1=fltarr(np,nstu)

	  bvars=fltarr(1,nsts)
	  us=fltarr(1,nsts)
	  vs=fltarr(1,nsts)
	  zs=fltarr(1,nsts)
	  Ts=fltarr(1,nsts)

	  madn=[3,4,1,2]
	  
	  a1=fltarr(nad,nad)
	  b1=fltarr(nad)

    	  dus=du
	
	  unith(*)=1.0
	  uniths(*)=1.0
          unitv(kb:kt)=1.0
	  unitv1(ks:kt)=1.0
;;;;;;;;;;
          unitv1(ks)=((budget(0,6)-p(ks))*100.0+dp/2.0)/dp
	
	  for mb=0,nvbudget_column-1 do begin
	    c(mb)=0
	    nterm=budget(mb,0)
	    for item=2,nterm do begin
	      c(mb)=c(mb)-budget(mb,item)
	    endfor
	  endfor
	  
	  for ist=0,nstu-1 do begin
	    for k=kb,kt do begin
	      unit(k,ist)=dus(0,k,ist)
	      r(k,ist)=dus(1,k,ist)
	      h(k,ist)=dus(2,k,ist)
	      u(k,ist)=dus(3,k,ist)
	      v(k,ist)=dus(4,k,ist)
	      z(k,ist)=dus(6,k,ist)
	      T(k,ist)=dus(5,k,ist)
	    endfor
	  endfor
	  
	  for ist=0,nsts-1 do begin
	    us(0,ist)=dss(3,ist)
	    vs(0,ist)=dss(4,ist)
	    zs(0,ist)=dss(6,ist)
	    Ts(0,ist)=dss(5,ist)
	  endfor
	  
	  for ist=0,nstu-1 do begin
	    for k=kb,kt do begin
	      dbdvar(0,0,k,ist)=unit(k,ist)*divu(k,ist)*dp/g
	      dbdvar(0,1,k,ist)=unit(k,ist)*divv(k,ist)*dp/g
	      dbdvar(1,0,k,ist)=r(k,ist)*divu(k,ist)*dp/g
	      dbdvar(1,1,k,ist)=r(k,ist)*divv(k,ist)*dp/g
	      dbdvar(1,2,k,ist)=(u(k,ist)*divu(k,ist)+v(k,ist)*divv(k,ist))*dp/g
	      dbdvar(2,0,k,ist)=h(k,ist)*divu(k,ist)*dp/g
	      dbdvar(2,1,k,ist)=h(k,ist)*divv(k,ist)*dp/g
	      dbdvar(2,3,k,ist)=(u(k,ist)*divu(k,ist)+v(k,ist)*divv(k,ist))*dp/g
	      dbdvar(3,0,k,ist)=(2.0*u(k,ist)*divu(k,ist)+v(k,ist)*divv(k,ist))*dp/g
 	      dbdvar(3,1,k,ist)=(u(k,ist)*divv(k,ist)-f(k,ist)/nstu)*dp/g
              dbdvar(3,3,k,ist)=(kt+1-k)*Rd*dp/100.0/p(k)*dzdx(k,ist)*dp/g
	      dbdvar(4,0,k,ist)=(v(k,ist)*divu(k,ist)+f(k,ist)/nstu)*dp/g
	      dbdvar(4,1,k,ist)=(u(k,ist)*divu(k,ist)+2.0*v(k,ist)*divv(k,ist))*dp/g
              dbdvar(4,3,k,ist)=(kt+1-k)*Rd*dp/100.0/p(k)*dzdy(k,ist)*dp/g
	    endfor
	  endfor
	  
	  for mb=0,4 do begin
	    for ist=0,nstu-1 do begin
	      for k=kb,kt do begin
	        bvar(k,ist)=dus(mb,k,ist)
	      endfor
	    endfor
	    
	    for ist=0,nsts-1 do begin
	      bvars(0,ist)=dss(mb,ist)
	    endfor

	    for mb1=0,4 do begin
	      for ist=0,nstu-1 do begin
	        for k=kb,kt do begin
	          u1(k,ist)=-dbdvar(mb1,0,k,ist)/2.0/wvar(3,k,ist)
	          v1(k,ist)=-dbdvar(mb1,1,k,ist)/2.0/wvar(4,k,ist)
                      r2(k)=0.0
                      h2(k)=-dbdvar(mb1,3,k,ist)/2.0/wvar(2,k,ist)
	        endfor
		height,np,kb,kt,p,h2,r2,z2
                z1(*,ist)=z2(*)
	      endfor
	      diverg,unith,bvar,u1,v1,divu,divv,div2
	      sdiv2=transpose(div2)#unitv*dp/g
	      a(mb,mb1)=sdiv2(0)
	      if(abs(a(mb,mb1)) gt 100) then begin
	        print,'!!!!!!!!!!',mb,mb1,a(mb,mb1)
;	        stop
	      endif
	      if(mb eq 3) then begin
	        fcorlx,unith,nstu,f,v1,fcx1
	        sfcx1=transpose(fcx1)#unitv*dp/g
                fpgd,unith,z1,dzdx,fpx1
                sfpx1=transpose(fpx1)#unitv*dp/g
                a(3,mb1)=a(3,mb1)-sfcx1(0)-sfpx1(0)
	      endif
	      if(mb eq 4) then begin
	        fcorly,unith,nstu,f,u1,fcy1
	        sfcy1=transpose(fcy1)#unitv*dp/g
                fpgd,unith,z1,dzdy,fpy1
                sfpy1=transpose(fpy1)#unitv*dp/g
                a(4,mb1)=a(4,mb1)-sfcy1(0)-sfpy1(0)
	      endif
	    endfor
	    
	    diverg,unith,bvar,u,v,divu,divv,div1
	    diverg,uniths,bvars,us,vs,divus,divvs,divs
	    itps,ks,kb,div1,divs(0)
	    for k=ks,kt do begin
	      divbvar(mb,k)=div1(k)/ad(mb)
	    endfor
	    sdiv1=transpose(div1)#unitv1*dp/g
	    b(mb)=sdiv1(0)+c(mb)
	    nterm=budget(mb,0)
	    budget(mb,nterm+1)=-sdiv1(0)
	  endfor

	  diverg,unith,T,u,v,divu,divv,div1
	  diverg,uniths,Ts,us,vs,divus,divvs,divs
	  itps,ks,kb,div1,divs(0)
          for k=ks,kt do begin
            divbvar(5,k)=div1(k)
          endfor
	  	  
          for mvar=0,nadvar-1 do begin
	    m=madn(mvar)
            for mb1=0,4 do begin
	      for ist=0,nstu-1 do begin
	        for k=kb,kt do begin
	          bvar1(k,ist)=-dbdvar(mb1,mvar,k,ist)/2.0/wvar(m,k,ist)
		endfor
              endfor
	      diverg,unith,bvar1,u,v,divu,divv,div1
	      sdiv1=transpose(div1)#unitv*dp/g
	      a(m,mb1)=a(m,mb1)+sdiv1(0)
	    endfor
	  endfor

	  fcorlx,unith,nstu,f,v,fcx1
	  fcorlx,uniths,nsts,fs,vs,fcxs
	  itps,ks,kb,fcx1,fcxs(0)
	  sfcx1=transpose(fcx1)#unitv1*dp/g
	  fpgd,unith,z,dzdx,fpx1
	  fpgd,uniths,zs,dzdxs,fpxs
	  itps,ks,kb,fpx1,fpxs(0)
	  sfpx1=transpose(fpx1)#unitv1*dp/g
	  b(3)=b(3)-sfcx1(0)-sfpx1(0)
	  fcorly,unith,nstu,f,u,fcy1
	  fcorly,uniths,nsts,fs,us,fcys
	  itps,ks,kb,fcy1,fcys(0)
	  sfcy1=transpose(fcy1)#unitv1*dp/g
	  fpgd,unith,z,dzdy,fpy1
	  fpgd,uniths,zs,dzdys,fpys
	  itps,ks,kb,fpy1,fpys(0)
	  sfpy1=transpose(fpy1)#unitv1*dp/g
	  b(4)=b(4)-sfcy1(0)-sfpy1(0)
	  budget(3,5)=sfcx1(0)
	  budget(3,6)=sfpx1(0)
	  budget(4,5)=sfcy1(0)
	  budget(4,6)=sfpy1(0)
	  fcx=fcx1
	  fcy=fcy1
	  fpx=fpx1
	  fpy=fpy1
	  for mb=0,4 do begin
	    budget(mb,1)=-b(mb)
	  endfor
	  b=-b
	  
	  a1(*)=0.0
	  b1(*)=0.0
	  for i1=0,nad-1 do begin
	    for i2=0,nad-1 do begin
	      a1(i1,i2)=a(i1,i2)
	    endfor
	    b1(i1)=b(i1)
	  endfor
;;	  print,'a1'
;;	  print,a1
	  a2=a1
;;	  print,'b1'
;;	  print,b1
	  
;;	  print,''
	  if(nad lt 2) then begin
	    ld1=fltarr(nad)
	    ld1(0)=b1(0)/a1(0,0)
;;	    print,'ld1'
;;	    print,ld1
	    print,'b2'
	    print,a2#ld1
	   endif else begin
	    nr_ludcmp,a1,index
            ld1=nr_lubksb(a1,index,b1)
;;	    print,'ld1'
;;	    print,ld1
	    print,'b2'
	    print,a2#ld1
	  endelse
	  
	  for i=0,nad-1 do begin
	    ld(i)=ld1(i)
	  endfor
	  
	  for ist=0,nstu-1 do begin
	    for k=kb,kt do begin
	      for mad=0,nadvar-1 do begin
	        m=madn(mad)
	        for mb=0,nad-1 do begin
	          dus(m,k,ist)=dus(m,k,ist)-ld(mb)*dbdvar(mb,mad,k,ist)$
                                                   /2.0/wvar(m,k,ist)
	        endfor
	      endfor
	    endfor
	  endfor

          for m=0,nvbudget_column-1 do begin
            for k=0,np-1 do begin
              for ist=0,nstu-1 do begin
                dus(m,k,ist)=dus(m,k,ist)/ad(m)
              endfor
            endfor
            nterm=budget(m,0)
            for iterm=1,nterm+1 do begin
              budget(m,iterm)=budget(m,iterm)/ad(m)
            endfor
          endfor

	end
