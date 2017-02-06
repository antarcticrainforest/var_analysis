      pro cdist,x1,y1,x2,y2,d
       pi=3.1416/180.0
       radius=6400.0
       dx=(x2-x1)*pi*cos(y1*pi)*radius
       dy=(y2-y1)*pi*radius
       d=(dx*Dx+dy*dy)^0.5
      end
   
     pro windowxy, nx,ny,x,y,x1,y1,nd,id,jd,dd,xx
      y2=y
      x2=x
      y2(*,*)=y1
      x2(*,*)=x1
      cdist,x2,y2,x,y,d

      nn=nx*ny
      iid=intarr(nn)
      jjd=iid
      ddd=fltarr(nn)
      xx2=xx
jump1:
      nd=0
      for i=0,nx-1 do begin
        for j=0,ny-1 do begin
           if(d(i,j) le xx2)then begin
           iid(nd)=i
           jjd(nd)=j
           ddd(nd)=d(i,j)
           nd=nd+1
           endif
         endfor
      endfor 
      if(nd eq 0)then begin
       xx2=xx2*1.2
       goto,jump1
      endif

      id=intarr(nd)
      jd=id
      dd=fltarr(nd)
      id(*)=iid(0:nd-1)
      jd(*)=jjd(0:nd-1)
      dd(*)=ddd(0:nd-1)
 end


     pro windowt, nt,t,xt,i1,i2
           if(xt le t(0)) then begin & $
           i1=0          & $
           i2=0          & $
         goto, jump1     & $
      endif      & $
      if(xt ge t(nt-1)) then begin & $
           i1=nt-1      & $
           i2=i1         & $
           goto, jump1   & $
      endif           & $
      for i=1,nt-1 do begin & $
      if(xt gt t(i-1) and xt le t(i)) then begin & $
       i1=i-1       & $
       i2=i         & $
       goto, jump1  & $
       endif        & $
      endfor        & $
jump1:
   end


     pro windowp, nt,t,xt,i1,i2
           if(xt ge t(0)) then begin & $
           i1=0          & $
           i2=0          & $
         goto, jump1     & $
      endif      & $
      if(xt le t(nt-1)) then begin & $
           i1=nt-1      & $
           i2=i1         & $
           goto, jump1   & $
      endif           & $
      for i=1,nt-1 do begin & $
      if(xt lt t(i-1) and xt ge t(i)) then begin & $
       i1=i-1       & $
       i2=i         & $
       goto, jump1  & $
       endif        & $
      endfor        & $
jump1:
   end

     pro windowt2,n2,nt,t,xt,i1,i2
       windowt,nt,t,xt,i1,i2
        i1=i1-n2
        i2=i2+n2
        if(i1 lt 0)then begin
         i1=0
        endif
        if(i2 gt nt-1)then begin
         i2=nt-1
        endif
end
     pro windowt3,n2,n,i,it1,it2
      it1=i-n2 & it2=i+n2
      if(it1 lt 0)then begin
       it1=0
      endif
      if(it2 gt n-1)then begin
       it2=n-1
      endif
end

      pro barns1,nst,x,d,xl,dd
        y=(x/xl)
 
        w=exp(-y^2)
        totalw=total(w)
        w=w/totalw
        dd=total(w*d)
      end

; ------------------------------------

      pro barns1N,x,wet,d,dd
        ;normalized x
        y=x^2
        w=x*0.0

        if(min(y) ge 20.0)then begin
          y(*)=1.0
        endif

        jj=where(y le 20.,count)
        w(jj) = exp(-y(jj)) 
        
        w=w*wet
        totalw=total(w)
        w=w/totalw
        dd=total(w*d)
      end

      pro cressman1xie,dx,dt,d,xlx,xlt,cw,m,ilev,ist,i
         
        w=(dx/xlx)^2+(dt/xlt)^2
        if(min(w) gt 10.0)then begin
         w(*)=1.0
        endif
      
        if(min(w) le 20.)then begin 
          w(where(w le 20.))=exp(-w(where(w le 20.)))
        endif
       
        if(max(w) gt 20)then begin
          w(where(w gt 20.))=0.0
        endif

        totalw=total(w)
        
        w=w/totalw
    
       cw=total(w*d)
       if(m eq 0 and ilev eq 28 and ist eq 4 and i eq 12) then begin
          stop  
       endif
   end
 
     pro cressman1,dx,dt,d,xlx,xlt,cw
         
        w=(dx/xlx)^2+(dt/xlt)^2
        if(min(w) gt 10.0)then begin
         w(*)=1.0
        endif
      
        if(min(w) le 20.)then begin 
          w(where(w le 20.))=exp(-w(where(w le 20.)))
        endif
       
        if(max(w) gt 20)then begin
          w(where(w gt 20.))=0.0
        endif

        totalw=total(w)
        
        w=w/totalw
    
       cw=total(w*d)
         
   end
      pro cressman2x,ns,dds,dts,dws,np,ddp,dtp,dwp,xlx,xlt,rw,cw

        if(ns eq 0 and np eq 0)then begin
          cw=0.0
          goto,jump1
        endif

        if(ns ne 0 and np eq 0)then begin
          cressman1,dds,dts,dws,xlx,xlt,cw
          goto,jump1
        endif
         
        if(np ne 0 and ns eq 0)then begin
          cressman1,ddp,dtp,dwp,xlx,xlt,cw
          goto,jump1
        endif
         
        dx=[dds,ddp]
        dt=[dts,dtp]
        d =[dws,dwp]

        w=(dx/xlx)^2+(dt/xlt)^2
        if(min(w) gt 5.0)then begin
         w(*)=1.0
        endif
        print,w

        w1=w
        if(min(w1) le 20.)then begin 
        w(where(w1 le 20))=exp(-w1(where(w1 le 20)))
        endif

        if(max(w1) gt 20.)then begin 
        w(where(w1 gt 20))=0.0
        endif

        w(0:ns-1)=rw*w(0:ns-1)     ; more weight for soudning

        totalw=total(w)
        
        w=w/totalw
       cw=total(w*d)

jump1:       
stop
 end  
      pro cressman2xie,ns,dds,dts,dws,np,ddp,dtp,dwp,xlx,xlt,rw,cw,m,ilev,ist,i

        if(ns eq 0 and np eq 0)then begin
          cw=0.0
          goto,jump1
        endif

        if(ns ne 0 and np eq 0)then begin
          cressman1xie,dds,dts,dws,xlx,xlt,cw,m,ilev,ist,i
          goto,jump1
        endif
         
        if(np ne 0 and ns eq 0)then begin
          cressman1xie,ddp,dtp,dwp,xlx,xlt,cw,m,ilev,ist,i
          goto,jump1
        endif
         
        dx=[dds,ddp]
        dt=[dts,dtp]
        d =[dws,dwp]

        w=(dx/xlx)^2+(dt/xlt)^2
        if(min(w) gt 5.0)then begin
         w(*)=1.0
        endif

        w1=w
        if(min(w1) le 20.)then begin 
        w(where(w1 le 20))=exp(-w1(where(w1 le 20)))
        endif

        if(max(w1) gt 20.)then begin 
        w(where(w1 gt 20))=0.0
        endif

        w(0:ns-1)=rw*w(0:ns-1)     ; more weight for soudning

        totalw=total(w)
        
        w=w/totalw
       cw=total(w*d)
jump1:       
 end  
        
      pro cressman2,ns,dds,dts,dws,np,ddp,dtp,dwp,xlx,xlt,rw,cw

        if(ns eq 0 and np eq 0)then begin
          cw=0.0
          goto,jump1
        endif

        if(ns ne 0 and np eq 0)then begin
          cressman1,dds,dts,dws,xlx,xlt,cw
          goto,jump1
        endif
         
        if(np ne 0 and ns eq 0)then begin
          cressman1,ddp,dtp,dwp,xlx,xlt,cw
          goto,jump1
        endif
         
        dx=[dds,ddp]
        dt=[dts,dtp]
        d =[dws,dwp]

        w=(dx/xlx)^2+(dt/xlt)^2
        if(min(w) gt 5.0)then begin
         w(*)=1.0
        endif

        w1=w
        if(min(w1) le 20.)then begin 
        w(where(w1 le 20))=exp(-w1(where(w1 le 20)))
        endif

        if(max(w1) gt 20.)then begin 
        w(where(w1 gt 20))=0.0
        endif

        w(0:ns-1)=rw*w(0:ns-1)     ; more weight for soudning

        totalw=total(w)
        
        w=w/totalw
       cw=total(w*d)
jump1:       
 end  
     pro rhlimit,d,m,np,nst,nt,fac
      for i=0,np-1 do begin
       for j=0,nst-1 do begin
         for k=0,nt-1 do begin
           if(d(m,i,j,k) lt 0)then begin
            d(m,i,j,k)=0.001
           endif
           if(d(m,i,j,k)/fac gt 1.0)then begin
            d(m,i,j,k)=1.0*fac
           endif
         endfor
        endfor
      endfor
end

     pro smooth,ns,d,n,dw
      dw=fltarr(n)
      ns2=ns/2
      for i=0,n-1 do begin
        windowt3,ns2,n,i,it1,it2
        n3=it2-it1+1
        d3=fltarr(n3)  
         w3=d3
        d3=d(it1:it2)
         for j=0,n3-1 do begin
           w3(j)=(j*(n3-1-j))^0.25+1.0
         endfor
         totw=total(w3)
         w3=w3/totw
         cw=0.0
         cw2=0.0
          for j=0,n3-1 do begin
           if(d3(j) gt -2999.)then begin
           cw=cw+d3(j)*w3(j)
           cw2=cw2+w3(j)
           endif
          endfor
      
         if(cw2 eq 0.0)then begin
           dw(i)=-9999.0
           goto,jump1
         endif
           dw(i)=cw/cw2
jump1:

      endfor
end

     pro barnes2,dx,dt,d,xlx,xlt,rw,cw
         
        w=(dx/xlx)^2+(dt/xlt)^2
        if(min(w) gt 10.0)then begin
         w(*)=1.0
        endif
      
        if(min(w) le 20.)then begin 
        w(where(w le 20.))=exp(-w(where(w le 20.)))
        endif
       
        if(max(w) gt 20)then begin
        w(where(w gt 20.))=0.0
        endif

        w=w*rw
        totalw=total(w)
        
        w=w/totalw
    
       cw=total(w*d)
         
   end


