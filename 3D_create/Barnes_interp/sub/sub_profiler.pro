	pro extraprof,n,u,m,ul,uh

         dw1=fltarr(n)
         dw1(*)=-9999.0
         dw=dw1
         
         dw2=dw1

         dw1(0:m-1)=ul
         dw2(0:m-1)=reverse(uh)
         dw2=reverse(dw2)

         for i=0,n-1 do begin
           if(dw1(i) gt -9990. and dw2(i) gt -9990.)then begin
             dw(i)=0.5*(dw1(i)+dw2(i))
             goto,jump1
           endif

           if(dw1(i) gt -9990. and dw2(i) le -9990.)then begin
              dw(i)=dw1(i)
              goto,jump1
           endif

           if(dw2(i) gt -9990. and dw1(i) le -9990.)then begin
              dw(i)=dw2(i)
              goto,jump1
           endif
jump1:
         endfor

         u=dw
 end 
         
      pro calht, ps,n,p,t,rh,ht
      if(min(rh) le 0.0)then begin
       rh(where(rh le 0.0))=0.001
      endif
      if(max(rh) gt 1.0)then begin
       rh(where(rh gt 1.0))=1.0
      endif
        es=1.003*exp(53.67957-6743.769/T-4.8451*alog(T))

        Rd=287.04
        g=9.8
        Rv=461.50
        ep=Rd/Rv

        rs=ep*es/(p-es)
        r=rs*rh
      
         for i=0,n-1 do begin
           if(ps ge p(i))then begin
             ks=i
             goto,jump1
            endif
         endfor
jump1:
        R1=Rd*0.5*((1+r(ks-1)/ep)/(1+r(ks-1))+(1+r(ks)/ep)/(1+r(ks)))

        dz=R1/g*0.5*(T(ks-1)/p(ks-1)+T(ks)/p(ks))*(ps-p(ks))
        ht(ks)=dz

        height,np,ks,n-1,p,t,r,ht
         for i=0,ks-1 do begin
           ht(i)=0.0
         endfor
 end

      pro calht2, ps,zs,n,p,tc,r,ht,dew

      rh=r/100.0     
      t=tc+273.16 
      if(min(rh) le 0.0)then begin
       rh(where(rh le 0.0))=1.
      endif
      if(max(rh) gt 1.0)then begin
       rh(where(rh gt 1.0))=1.0
      endif

        es=1.003*exp(53.67957-6743.769/T-4.8451*alog(T))

        Rd=287.04
        g=9.8
        Rv=461.50
        ep=Rd/Rv

        rs=ep*es/(p-es)
        r=rs*rh
      
         for i=0,n-1 do begin
           if(ps ge p(i))then begin
             ks=i
             goto,jump1
            endif
         endfor
jump1:
        R1=Rd*0.5*((1+r(ks-1)/ep)/(1+r(ks-1))+(1+r(ks)/ep)/(1+r(ks)))
        dz=R1/g*0.5*(T(ks-1)/p(ks-1)+T(ks)/p(ks))*(ps-p(ks))
        ht(ks)=dz+zs
        height,np,ks,n-1,p,t,r,ht
         for i=1,ks-1 do begin    ; 0 already assigned surface value
           ht(i)=zs
         endfor
           ht(0)=zs
 end

        pro interpf,npf,hpf1,upf1,np,hpf2,upf2
         for i=0,np-1 do begin
            d=-9999.0
            upf2(i)=-9999.0
            for j=0,npf-2 do begin
              if(hpf2(i) ge hpf1(j) and hpf2(i) lt hpf1(j+1))then begin
                t_1=hpf1(j)
                t1=hpf1(j+1)
                d_1=upf1(j)
                d1=upf1(j+1)
                t=hpf2(i)
                itp_2point,t_1,d_1,t1,d1,-9990.0,t,d
                upf2(i)=d
              goto,jump1
              endif
             endfor
jump1:
         endfor
end
 

                           
