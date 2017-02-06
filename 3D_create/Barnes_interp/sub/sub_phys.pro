       pro s_r_to_T_z,p,ps,zs,s,r,T,z
; p ps in mb,zs in m
; column
; T (K)
; r (g/kg)
; s (K)
; z (m)

       n=n_elements(p)
       T=fltarr(n)
       z=T

       Cpd = 1005.7
       g2 = 2.0*9.8
       Rd=287.04
       Rw=461.50
       ep=Rd/Rw

       jj=where(p ge ps,count)
       j0=0
       z(0)=zs
       if(count gt 0)then begin
         z(jj)=zs
         T(jj) = s(0) - 9.8*Z(jj)/Cpd
         j0 = max(jj)
       endif 
        
       p1 = ps
       T1 = T(0)
       r1 = r(0)
       z1 = zs 

 
      for i=j0+1,n-1 do begin
         p2 = p(i)
         r2 = r(i)
       
         Rv=Rd*0.5*((1+r1/ep)/(1+r1)+(1+r2/ep)/(1+r2))

         a1 = Rv/2./Cpd*alog(p1/p2)
         T(i) = (s(i) - 9.8/Cpd*z1 - a1*T1)/(1.+a1)
         z(i)=z1 + Rv*(T1+T(i))/g2*alog(p1/p2)

         P1=P2
         R1=R2
         z1=z(i)
         T1=T(i)

       endfor
 
     end
;;;;;;;;;;;;;;;;;;;;;;;;;
       pro T_r_to_s_z,p,ps,zs,T,r,s,z
; p ps in mb,zs in m
; column
; T (K)
; r (g/kg)
; s (K)
; z (m)

       n=n_elements(p)
       s=fltarr(n)
       z=s

       Cpd = 1005.7
       g2 = 2.0*9.8
       Rd=287.04
       Rw=461.50
       ep=Rd/Rw

       jj=where(p ge ps,count)
       j0=0
       z(0)=zs
       if(count gt 0)then begin
         z(jj)=zs
         s(jj) = T(0) + 9.8*Z(jj)/Cpd
         j0 = max(jj)
       endif 
        
       p1 = ps
       T1 = T(0)
       r1 = r(0)
      

 
      for i=j0+1,n-1 do begin
         p2 = p(i)
         T2 = T(i)
         r2 = r(i)
         Rv=Rd*0.5*((1+r1/ep)/(1+r1)+(1+r2/ep)/(1+r2))
         z(i)=z(i-1) + Rv*(T1+T2)/g2*alog(p1/p2)
         T1=T2
         P1=P2
         R1=R2

         s(i) = T(i) + 9.8*z(i)/cpd
       endfor
 
     end
;;;;;;;;;;;;;;;;;;;;;;;;;
	pro height,np,ks,kt,p,T,r,z
	  Rd=287.04
	  Rw=461.50
	  ep=Rd/Rw
	  g=9.8
	  for k=ks+1,kt do begin
	    R1=Rd*0.5*((1+r(k-1)/ep)/(1+r(k-1))+(1+r(k)/ep)/(1+r(k)))
	    dz=R1/g*0.5*(T(k-1)/p(k-1)+T(k)/p(k))*(p(k-1)-p(k))
	    z(k)=z(k-1)+dz
	  endfor
	end
	

	
         pro calc_state,p,T,Td,e,z,r,rh,h,hdry,s	

	  Rd=287.04
	  Rv=461.50
	  ep=Rd/Rv
	  T0=273.15
	  cpd=1005.7
	  cpv=1870.0
	  cl=4190.0
	  Lv0=2.5E+6
	  g=9.8
	
	  es=1.003*exp(53.67957-6743.769/T-4.8451*alog(T))
	  if(e gt 99990.0) then begin
	    e=1.003*exp(53.67957-6743.769/Td-4.8451*alog(Td))
	  endif
	  r=ep*e/(p-e)
	  rh=e/es
	  Lv=Lv0+(cpv-cl)*(T-T0)
	  h=(cpd+r*cl)*T+Lv*r+(1.0+r)*g*z
	  hdry=cpd*T+g*z
	  s=cpd*alog(T)-Rd*alog(p)+Lv0*r/T-r*Rv*alog(rh)
	
	end

         pro calc_state2,p,T,Td,e,z,r,rh,h,hdry,s	

	  Rd=287.04
	  Rv=461.50
	  ep=Rd/Rv
	  T0=273.15
	  cpd=1005.7
	  cpv=1870.0
	  cl=4190.0
	  Lv0=2.5E+6
	  g=9.8
	
	  es=1.003*exp(53.67957-6743.769/T-4.8451*alog(T))

;	  if(e gt 99990.0) then begin
;	    e=1.003*exp(53.67957-6743.769/Td-4.8451*alog(Td))
;	  endif

          rs=ep*es/(p-es)
          r=rs*rh
;
;	  r=ep*e/(p-e)
;	  rh=e/es

	  Lv=Lv0+(cpv-cl)*(T-T0)
	  h=(cpd+r*cl)*T+Lv*r+(1.0+r)*g*z
	  hdry=cpd*T+g*z
	  s=cpd*alog(T)-Rd*alog(p)+Lv0*r/T-r*Rv*alog(rh)
	
	end


	pro calc_state0,p,T,wt,Tli1,Tli2,wvs,wv,wc,sd,sv,sc,s,Ts,ps,dzs,Tep,h
          Rd=287.04
          Rv=461.50
          ep=Rd/Rv

          T0=273.15
          cpd=1005.7
          cpv=1870.0
          cl=4190.0
          Llv0=2.501E+6
          Llv=(cpv-cl)*(T-T0)+Llv0
          Liv=2.834E+6

	  g=9.8

          esl=1.003*exp(53.67957-6743.769/T-4.8451*alog(T))
          esi=1.003*exp(23.33086-6111.72784/T+0.15215*alog(T))
          es1=1.003*exp(53.67957-6743.769/Tli1-4.8451*alog(Tli1))
          es2=1.003*exp(23.33086-6111.72784/Tli2+0.15215*alog(Tli2))
          Lc1=(cpv-cl)*(Tli1-T0)+Llv0
          Lc2=Liv
          if(T gt Tli1) then begin
            es=esl
            Lc=Llv
	    goto,jump1
          endif
	  if(T lt Tli2) then begin
            es=esi
            Lc=Liv
	    goto,jump1
          endif
          es=es1+(T-Tli1)/(Tli2-Tli1)*(es2-es1)
          Lc=Lc1+(T-Tli1)/(Tli2-Tli1)*(Lc2-Lc1)
jump1:    wvs=ep*es/(p-es)

          e=wt/(wt+ep)*p
          if(e ge es) then begin
            e=es
          endif
          pd=p-e
          wv=ep*e/pd
          wc=wt-wv

          s=((cpd+wt*cpv)*alog(T)-Rd*alog(pd)-wt*Rv*alog(e)-wc*Lc/T)/(1+wt)
          sd=cpd*alog(T)-Rd*alog(pd)
          sv=cpv*alog(T)-Rv*alog(e)
          sc=cpv*alog(T)-Rv*alog(es)-Lc/T

          Ts=2840.0/(3.5*alog(T)-alog(e)-4.805)+55.0
          ps=p*exp((cpd+wv*cpv)/Rd/(1.0+wv/ep)*alog(Ts/T))
          dzs=cpd/g*(1.0+wv*cpv/cpd)/(1.0+wv)*(T-Ts)

          Tep=T*(1000.0/p)^(0.2854*(1.0-0.28*wv)) $
               *exp(wv*(1.0+0.81*wv)*(3376.0/Ts-2.54))

          h=((cpd+wt*cpv)*T-wc*Lc)/(1+wt)

        end


	pro calc_s_Tep_h,p,T,wt,Tli1,Tli2,wvs,wv,wc,s,dsdT,Tep,h
          Rd=287.04
          Rv=461.50
          ep=Rd/Rv

          T0=273.15
          cpd=1005.7
          cpv=1870.0
          cl=4190.0
          Llv0=2.501E+6
          Llv=(cpv-cl)*(T-T0)+Llv0
          Liv=2.834E+6

          esl=1.003*exp(53.67957-6743.769/T-4.8451*alog(T))
          esi=1.003*exp(23.33086-6111.72784/T+0.15215*alog(T))
          es1=1.003*exp(53.67957-6743.769/Tli1-4.8451*alog(Tli1))
          es2=1.003*exp(23.33086-6111.72784/Tli2+0.15215*alog(Tli2))
          Lc1=(cpv-cl)*(Tli1-T0)+Llv0
          Lc2=Liv
          if(T gt Tli1) then begin
            es=esl
	    desdT=esl*(6743.769/T/T-4.8451/T)
            Lc=Llv
	    goto,jump1
          endif
	  if(T lt Tli2) then begin
            es=esi
	    desdT=esi*(6111.72784/T/T+0.15215/T)
            Lc=Liv
	    goto,jump1
          endif
          es=es1+(T-Tli1)/(Tli2-Tli1)*(es2-es1)
	  desdT=1.0/(Tli2-Tli1)*(es2-es1)
          Lc=Lc1+(T-Tli1)/(Tli2-Tli1)*(Lc2-Lc1)
jump1:    wvs=ep*es/(p-es)

          e=wt/(wt+ep)*p
	  dedT=0.0
          if(e ge es) then begin
            e=es
	    dedT=desdT
          endif
          pd=p-e
          wv=ep*e/pd
          wc=wt-wv

          s=((cpd+wt*cpv)*alog(T)-Rd*alog(pd)-wt*Rv*alog(e)-wc*Lc/T)/(1+wt)
	  dsdT=((cpd+wt*cpv)/T+(Rd/pd-wt*Rv/e)*dedT+wc*Lc/T/T)/(1+wt)
          Ts=2840.0/(3.5*alog(T)-alog(e)-4.805)+55.0
          Tep=T*(1000.0/p)^(0.2854*(1.0-0.28*wv)) $
               *exp(wv*(1.0+0.81*wv)*(3376.0/Ts-2.54))

          h=((cpd+wt*cpv)*T-wc*Lc)/(1+wt)

        end


	pro s_to_T,wt0,s0,p,Tli1,Tli2,T,wvs,wv,wc,Tro,dTrods,Tep,h
	  Rd=287.04
	  Rv=461.50
	  ep=Rd/Rv

	  i=0
	  T1=100.0
	  calc_s_Tep_h,p,T1,wt0,Tli1,Tli2,wvs,wv,wc,s1,dsdT,Tep,h
	  f1=s1-s0
	  T2=500.0
	  calc_s_Tep_h,p,T2,wt0,Tli1,Tli2,wvs,wv,wc,s2,dsdT,Tep,h
	  f2=s2-s0
jump1:	  T=(T1+T2)*0.5
	  calc_s_Tep_h,p,T,wt0,Tli1,Tli2,wvs,wv,wc,s,dsdT,Tep,h
	  f=s-s0
	  i=i+1
	  if(i gt 100) then begin
	    print,i,T,s0,s
	  endif
	  if(abs(s-s0) le 0.001) then begin
	    goto,jump2
	  endif
	  if(f*f1 gt 0.0) then begin
	    T1=T
	    f1=f
	  endif else begin
	    T2=T
	    f2=f
	  endelse
	  goto,jump1
jump2:    Tro=T*(1.0+wv/ep)/(1.0+wt0)
	  dTrods=1.0/dsdT*(1.0+wv/ep)/(1.0+wt0)

	end


	pro Tep_to_T,wt0,Tep0,p,Tli1,Tli2,T,wvs,wv,wc,Tro,s,h
	  Rd=287.04
	  Rv=461.50
	  ep=Rd/Rv

	  i=0
	  T1=100.0
	  calc_s_Tep_h,p,T1,wt0,Tli1,Tli2,wvs,wv,wc,s,dsdT,Tep1,h
	  f1=Tep1-Tep0
	  T2=500.0
	  calc_s_Tep_h,p,T2,wt0,Tli1,Tli2,wvs,wv,wc,s,dsdT,Tep2,h
	  f2=Tep2-Tep0
jump1:	  T=(T1+T2)*0.5
	  calc_s_Tep_h,p,T,wt0,Tli1,Tli2,wvs,wv,wc,s,dsdT,Tep,h
	  f=Tep-Tep0
	  i=i+1
	  if(i gt 100) then begin
	    print,i,T,Tep0,Tep
	  endif
	  if(abs(Tep-Tep0) le 0.001) then begin
	    goto,jump2
	  endif
	  if(f*f1 gt 0.0) then begin
	    T1=T
	    f1=f
	  endif else begin
	    T2=T
	    f2=f
	  endelse
	  goto,jump1
jump2:	  wc=0
	  wt=wv	  
	  Tro=T*(1.0+wv/ep)/(1.0+wt)

	end


	pro base_move,np,ks,kt,p,T,wt,s,Tep,Tli1,Tli2,Trom,dTrodsm,hm,wcm
	  s=fltarr(np)
	  Tep=fltarr(np)
	  Trom=fltarr(np,np)
	  dTrodsm=fltarr(np,np)
          hm=fltarr(np,np)
          wcm=fltarr(np,np)

          Rd=287.04
          Rv=461.50
          ep=Rd/Rv

          for k=ks,kt do begin
            calc_s_Tep_h,p(k),T(k),wt(k),Tli1,Tli2,wvs,wv,wc,s1,dsdT,Tep1,h
	    s(k)=s1
	    Tep(k)=Tep1
	  endfor

	  for k1=ks,kt do begin
	    for k2=k1,kt do begin
	      s_to_T,wt(k1),s(k1),p(k2),Tli1,Tli2,T1,wvs,wv,wc,Tro1,dTrods,Tep1,h
              Trom(k1,k2)=Tro1
              dTrodsm(k1,k2)=dTrods
	      hm(k1,k2)=h	      
	      wcm(k1,k2)=wc
	    endfor
	  endfor
	
	end


	pro calc_cape,np,ks,kt,p,Trom,dTrodsm,wcm,CAPE,dCAPEds,ktop,wc,dTro,CI
	  CI=fltarr(np)
	  CAPE=fltarr(np)
	  dCAPEds=fltarr(np,np)
          wc=fltarr(np)
          dTro=fltarr(np)
	  ktop=fltarr(np)
	  base=fltarr(np)
          dp=p(ks)-p(ks+1)

          Rd=287.04
	  for k=ks,kt-1 do begin
            CI(k)=0.0
            CAPE(k)=0.0
	    dCAPEds(k,k)=0.0
            for k1=kt,k+1,-1 do begin
	      dTro1=Trom(k,k1)-Trom(k1,k1)
	      dTro2=Trom(k,k1-1)-Trom(k1-1,k1-1)
              base(k1)=Rd*(dTro1/p(k1)+dTro2/p(k1-1))*0.5*dp
	      CI(k)=CI(k)+base(k1)
	    endfor
            for k1=kt,k+1,-1 do begin
	      dTro1=Trom(k,k1)-Trom(k1,k1)
              if(dTro1 ge 0.0) then begin
	        ktop(k)=k1
	  	wc(k)=wcm(k,k1)
                for k2=k1,k+1,-1 do begin
                  CAPE(k)=CAPE(k)+base(k2)
	          dCAPEds(k,k)=dCAPEds(k,k) $
                    +Rd*(dTrodsm(k,k2)/p(k2)+dTrodsm(k,k2-1)/p(k2-1))*0.5*dp
	        endfor
	        dCAPEds(k,k1)=-Rd*dTrodsm(k,k1)/p(k1)*0.5*dp
	        for k2=k1-1,k+1,-1 do begin
	          dCAPEds(k,k2)=-Rd*dTrodsm(k,k2)/p(k2)*0.5*dp
	        endfor
	        goto,jump1
              endif
	    endfor
jump1:	    dTro(k)=Trom(k,k+1)-Trom(k+1,k+1)
	  endfor

	end


        pro calc_APE,np,ks,kt,hm,wcm,APE,kbase,ktop,wc
	  hm1=hm
	  hm2=hm
	  wcm1=wcm
	  wcm2=wcm

	  APE=0.0
	  wc=0.0

	  sdh_min=0.0
	  k1_min=-1
	  k2_min=-1
          for k1=ks,kt-1 do begin
            for k2=ks+1,k1-1 do begin
              sdh=hm1(k1,k2)-hm1(k1,k1)
              kd=(k2-k1)/abs(k2-k1)
              for k3=k1,k2-kd,kd do begin
                sdh=sdh+hm1(k3+kd,k3)-hm1(k3+kd,k3+kd)
	      endfor
              if(sdh lt sdh_min) then begin
                sdh_min=sdh
                k1_min=k1
                k2_min=k2
              endif
	    endfor
            for k2=k1+1,kt do begin
              sdh=hm1(k1,k2)-hm1(k1,k1)
              kd=(k2-k1)/abs(k2-k1)
              for k3=k1,k2-kd,kd do begin
                sdh=sdh+hm1(k3+kd,k3)-hm1(k3+kd,k3+kd)
	      endfor
              if(sdh lt sdh_min) then begin
                sdh_min=sdh
                k1_min=k1
                k2_min=k2
              endif
	    endfor
	  endfor
          APE=-sdh_min/(kt-ks+1)
	  kbase=k1_min
	  ktop=k2_min
	  wc=wcm1(k1_min,k2_min)

        end


        pro calc_ds,np,ks,kt,s,ds,kmax,kmin

          smin=s(ks)
          for k=ks+1,kt do begin
            if(s(k) lt smin) then begin
              smin=s(k)
              kmin=k
            endif
	  endfor
	  smax=s(ks)
	  for k=ks,kmin do begin
            if(s(k) gt smax) then begin
              smax=s(k)
              kmax=k
            endif
	  endfor
	  ds=smax-smin	    

        end
