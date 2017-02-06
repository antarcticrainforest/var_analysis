;pro stationw,d,p,t,ist,iu,iv,tstart,tsteps=tsteps,itemp=itemp,iq=iq,itiq
;  d(nv,np,nst,nt)
;pro spatialw,d,p,t,lon,lat,pre,tstart,iu,iv,itemp=itemp,iq=iq,it,iq
;  d(nv,np,nst,nt)
;pro stationsw,d,p,t,lon,lat,iu,iv,tstart,ists,$
;     iframe=iframe,tsteps=tsteps,itemp=itemp,iq=iq,itiq,size=size
; ists tells the order of connecting stations, size is the wind size
;  d(nv,np,nst,nt)
; ---- add diagnostics
;pro stationsw2,d,p,t,lon,lat,iu,iv,tstart,ists,$
;     iframe=iframe,tsteps=tsteps,itemp=itemp,iq=iq,itiq,size=size
; ists tells the order of connecting stations, size is the wind size
;  d(nv,np,nst,nt)

;examples in run_eval.pro

function strdigit,afloat,ndigit
  temp=strtrim(afloat,2)
  n1 = strpos(temp,'.')+ndigit+1
  if(ndigit eq 0)then ndigit=n1-1
  temp = strmid(temp,0,n1)
  return, temp
end
  
pro stationw,d,p,t,ist,iu,iv,tstart,tsteps=tsteps,$
     itemp=itemp,iq=iq,iwind=iwind,itiq,title=title
;  d(nv,np,nst,nt)
      if(not keyword_Set(title))then title=''

      np = n_elements(p)
      nt = n_elements(t)

      if(not keyword_set(tsteps))then tsteps = 8    ; one day default
      n1 = min(where(t ge tstart)) > 0
      n2 = n1+tsteps < nt-1

      ddx = (t(n2)-t(n1))/(n2-n1+1) 
      if(n1 eq n2) then ddx = 0.5 

      x1 = t(n1)-ddx
      x2 = t(n2)+ddx

      ptop = (fix(p(np-1))/100)*100
      pbottom = (fix(p(0)+99.9)/100) *100

      plot,[x1,x2],[ptop,pbottom],xrange=[x1,x2],yrange=[ptop,pbottom], $
                  /nodata,yticks=1,xticks=0,xstyle=1,ystyle=1,ycharsize=0.1,$
             color=19,title=title


      for k=0,np-1 do begin
        junk = strtrim(p(k),2)
        junk2 = strpos(junk,'.')        
          xyouts,x1,pbottom-p(k)+ptop,strmid(junk,0,junk2)+'-',alignment = 1.0,$
            color=19
			
      endfor

; --------- contour drawn first
     if(keyword_set(itemp))then begin
         dw=fltarr(n2-n1+1,np)
          for k=0,np-1 do begin
           for i=0,n2-n1 do begin
            dw(i,k)=d(itiq[0],k,ist,i+n1)
; calculate departure                
          endfor
            dw1=reform(dw(*,k))
            nw1=where(dw1 ge -200.,count)
            wbar=0
            if(count gt 0)then begin
               wbar=total(dw1(nw1))/n_elements(nw1)
               dw1(nw1)=dw1(nw1)-wbar
            endif
            dw(*,k)=dw1
          endfor

         xp = t(n1:n2)
         yp = pbottom - p + ptop
         levels=indgen(55)*2 - 80.
          nw1=where(dw ge -2000.,count)
          if(count gt 0)then begin
             if(min(dw(nw1)) le -60)then begin
               levels=indgen(51)-25.
             endif
          endif
         c_labels=indgen(n_elements(levels))*0+1

         contour,dw,xp,yp,levels=levels,max_value=100,$
          min_value=-100,c_labels=c_labels,/overplot,color=1
     endif

     if(keyword_set(iq))then begin
         dw=fltarr(n2-n1+1,np)
         for i=0,n2-n1 do begin
          for k=0,np-1 do begin
           dw(i,k)=d(itiq[1],k,ist,i+n1)
          endfor
          endfor

         levels= [indgen(8)*0.1+0.1,indgen(20)+1.]
         if(max(Dw) gt 50.)then begin   ;relative humidity
           levels=(indgen(10)+1)*10
         endif
         c_labels = indgen(n_elements(levels))*0+1
         xp = t(n1:n2)
         yp = pbottom - p + ptop
         
         contour,dw,xp,yp,levels=levels, $
            max_value=100.,min_value=-200.,c_labels=c_labels,/overplot,color=4
     endif

; -----------------------------------------------------     
       for i=n1,n2 do begin   
        x=t(i)
 ;       xyouts,x,0,strtrim(x,2),alignment = 0.5

        for k=0,np-1 do begin
          y=pbottom - p(k) + ptop
        
           if(keyword_Set(itemp) )then begin
              if(abs(d(itiq[0],k,ist,i)) le 1200) then $ 
              xyouts,x,y,' '+strdigit(d(itiq[0],k,ist,i),1),alignment=0.0 $
                 ,charsize = !p.charsize*0.2 $
              else xyouts,x,y,' -',color=1,alignment = 0.0
           endif

           if(keyword_Set(iq))then begin
              if(abs(d(itiq[1],k,ist,i)) le 1200) then $ 
              xyouts,x,y,strdigit(d(itiq[1],k,ist,i),1)+' ',alignment=1.0 $
                 ,charsize = !p.charsize*0.5 $
              else xyouts,x,y,'- ',color=1,alignment=1.0

           endif  

         if(not keyword_set(iwind))then $
           wind,x,y,d(iu,k,ist,i),d(iv,k,ist,i),2,size=2

             
        endfor
       endfor
        
end          
   

pro spatialw,d,p,t,lon,lat,pre,tstart,iu,iv,$
        itemp=itemp,iq=iq,iframe=iframe,itiq,ists
;  d(nv,np,nst,nt)

      np = n_elements(p)
      nt = n_elements(t)
      nst = n_elements(lon)
      nstc = n_elements(ists)

   ;   if(not keyword_set(tsteps))then tsteps = 8    ; one day default
   ;   n1 = min(where(t ge tstart)) > 0
   ;   n2 = n1+tsteps < nt-1

      xmax = max(lon)
      xmin = min(lon)
      ymax = max(lat)
      ymin = min(lat)

      ddx = (xmax-xmin)/n_elements(lon)       
      x1 = xmin-ddx
      x2 = xmax+ddx

      ddy = (ymax-ymin)/n_elements(lat)       
      y1 = ymin-ddy
      y2 = ymax+ddy

      plot,[x1,x2],[y1,y2],xrange=[x1,x2],yrange=[y1,y2], $
                  /nodata,yticks=0,xticks=0,xstyle=0,ystyle=0

	if(keyword_Set(iframe))then begin
           x = lon[ists[0]]          
           y = lat[ists[0]]
          for ist=1,nstc-1 do begin
           x1 =lon[ists[ist]]        
           y1 = lat[ists[ist]]
           oplot,[x,x1],[y,y1],color=5
           x=x1
           y=y1
          endfor
           x1 =lon[ists[0]]          
           y1 =lat[ists[0]]
           oplot,[x,x1],[y,y1],color=5
        endif
            


     ; for k=0,np-1 do begin
     ;   junk = strtrim(p(k),2)
     ;   junk2 = strpos(junk,'.')        
     ;     xyouts,x1,pbottom-p(k)+ptop,strmid(junk,0,junk2)+'-',alignment = 1.0
			
     ; endfor

     i = where(abs(t-tstart) eq min(abs(t-tstart)) )
     k = where(abs(p-pre) eq min(abs(p-pre))  )

       for ist= 0,nst-1 do begin 
          x=lon(ist)
          y=lat(ist)

           if(keyword_Set(itemp) )then begin
              temp =d[itiq[0],k,ist,i]
              temp = temp[0] 
              temp2 = strdigit(temp,1)
              if(abs(temp) le 1200) then $ 
              xyouts,x,y,' '+temp2,alignment=0.0 $
                 ,charsize = !p.charsize*0.2 $
              else xyouts,x,y,' -',color=1,alignment = 0.0
           endif

           if(keyword_Set(iq))then begin
              temp =d[itiq[1],k,ist,i]
              temp = temp[0] 
              temp2 = strdigit(temp,1)
              if(abs(temp) le 1200) then $ 
              xyouts,x,y,temp2+' ',alignment=1.0 $
                 ,charsize = !p.charsize*0.5,color=4 $
              else xyouts,x,y,'- ',color=1,alignment=1.0

           endif  
         
           uu=d[iu,k,ist,i]
           vv=d[iv,k,ist,i]
           
           wind,x,y,uu[0],vv[0],2,size=3

            
       endfor
  
        
end          
   

pro stationsw,d,p,t,lon,lat,iu,iv,tstart,ists,$
     iframe=iframe,tsteps=tsteps,itemp=itemp,iq=iq,iwind=iwind,itiq,size=size,$
     title=title
   if(not keyword_Set(title)) then title=''

; ists tells the order of connecting stations
;  d(nv,np,nst,nt)

      np = n_elements(p)
      nt = n_elements(t)
      nst = n_elements(lon)
      nstc = n_elements(ists)

      if(not keyword_set(tsteps))then tsteps = 2    ; half day default
      n1 = min(where(t ge tstart)) > 0
      n2 = n1+tsteps < nt-1

      ddx = (t(n2)-t(n1))/(n2-n1+1) 
      if(n1 eq n2) then ddx = (t(2)-t(1)) /2. 
      
      ddx2=( t(2)-t(1) )*0.5
      ddy2= (p(1)-p(2) )*0.5
      lonc = (max(lon)+min(lon))/2.0
      latc = (max(lat)+min(lat))/2.0
      dlon = max(lon) - min(lon)
      dlat = max(lat) - min(lat)

      x1 = t(n1)-ddx
      x2 = t(n2)+ddx

      ptop = (fix(p(np-1))/100)*100
      pbottom = (fix(p(0)+99.9)/100) *100

      plot,[x1,x2],[ptop,pbottom],xrange=[x1,x2],yrange=[ptop,pbottom], $
                  /nodata,yticks=1,xticks=0,xstyle=1,ystyle=1,ycharsize=0.1,color=19,xticklen=0,title=title

      for k=0,np-1 do begin
        junk = strtrim(p(k),2)
        junk2 = strpos(junk,'.')        
          xyouts,x1,pbottom-p(k)+ptop,strmid(junk,0,junk2)+'-',alignment = 1.0,color=19
			
      endfor

     
       for i=n1,n2 do begin   
        xc=t(i)
        for k=0,np-1 do begin
          yc=pbottom - p(k) + ptop

                          ;this is to plot the connecting boundary
	if(keyword_Set(iframe))then begin
           x = xc + ddx2*(lon[ists[0]]-lonc)/dlon           
           y = yc + ddy2*(lat[ists[0]]-latc)/dlat
          for ist=1,nstc-1 do begin
           x1 = xc + ddx2*(lon[ists[ist]]-lonc)/dlon           
           y1 = yc + ddy2*(lat[ists[ist]]-latc)/dlat
           oplot,[x,x1],[y,y1],color=5
           x=x1
           y=y1
          endfor
           x1 = xc + ddx2*(lon[ists[0]]-lonc)/dlon           
           y1 = yc + ddy2*(lat[ists[0]]-latc)/dlat
           oplot,[x,x1],[y,y1],color=5
        endif
            

          for ist = 0,nst-1 do begin
           x = xc + ddx2*(lon(ist)-lonc)/dlon           
           y = yc + ddy2*(lat(ist)-latc)/dlat

           if(keyword_Set(itemp) )then begin
              if(abs(d(itiq[0],k,ist,i)) le 1200) then $ 
              xyouts,x,y,' '+strdigit(d(itiq[0],k,ist,i),1),alignment=0.0 $
                 ,charsize = !p.charsize*0.2,color=4 $
              else xyouts,x,y,' -',color=19,alignment = 0.0
           endif

           if(keyword_Set(iq))then begin
              if(abs(d(itiq[1],k,ist,i)) le 1200) then $ 
              xyouts,x,y,strdigit(d(itiq[1],k,ist,i),1)+' ',alignment=1.0 $
                 ,charsize = !p.charsize*0.5,color=4 $
              else xyouts,x,y,'- ',color=2,alignment=1.0

           endif  

           if(not keyword_Set(iwind))then $
           wind,x,y,d(iu,k,ist,i),d(iv,k,ist,i),2,size=size

         endfor             
        endfor
       endfor
        
end          
   


pro stationsw2,d,p,t,lon,lat,iu,iv,tstart,ists,$
     iframe=iframe,tsteps=tsteps,itemp=itemp,iq=iq,itiq,size=size

; ists tells the order of connecting stations
;  d(nv,np,nst,nt)

      np = n_elements(p)
      nt = n_elements(t)
      nst = n_elements(lon)
      nstc = n_elements(ists)

      if(not keyword_set(tsteps))then tsteps = 2    ; half day default
      n1 = min(where(t ge tstart)) > 0
      n2 = n1+tsteps < nt-1

      ddx = (t(n2)-t(n1))/(n2-n1+1) 
      if(n1 eq n2) then ddx = (t(2)-t(1)) /2. 
      
      ddx2=( t(2)-t(1) )*0.5
      ddy2= (p(1)-p(2) )*0.5
      lonc = (max(lon)+min(lon))/2.0
      latc = (max(lat)+min(lat))/2.0
      dlon = max(lon) - min(lon)
      dlat = max(lat) - min(lat)

      x1 = t(n1)-ddx
      x2 = t(n2)+ddx


      ycoord,p,x1,x2,ptop,pbottom


       for i=n1,n2 do begin   
        xc=t(i)
        for k=0,np-1 do begin
          yc=pbottom - p(k) + ptop

                          ;this is to plot the connecting boundary
	if(keyword_Set(iframe))then begin
           x = xc + ddx2*(lon[ists[0]]-lonc)/dlon           
           y = yc + ddy2*(lat[ists[0]]-latc)/dlat
          for ist=1,nstc-1 do begin
           x1 = xc + ddx2*(lon[ists[ist]]-lonc)/dlon           
           y1 = yc + ddy2*(lat[ists[ist]]-latc)/dlat
           oplot,[x,x1],[y,y1],color=5
           x=x1
           y=y1
          endfor
           x1 = xc + ddx2*(lon[ists[0]]-lonc)/dlon           
           y1 = yc + ddy2*(lat[ists[0]]-latc)/dlat
           oplot,[x,x1],[y,y1],color=5
        endif
            

          for ist = 0,nst-1 do begin
           x = xc + ddx2*(lon(ist)-lonc)/dlon           
           y = yc + ddy2*(lat(ist)-latc)/dlat

           if(keyword_Set(itemp) )then begin
              if(abs(d(itiq[0],k,ist,i)) le 1200) then $ 
              xyouts,x,y,' '+strdigit(d(itiq[0],k,ist,i),1),alignment=0.0 $
                 ,charsize = !p.charsize*0.2 $
              else xyouts,x,y,' -',color=1,alignment = 0.0
           endif

           if(keyword_Set(iq))then begin
              if(abs(d(itiq[1],k,ist,i)) le 1200) then $ 
              xyouts,x,y,strdigit(d(itiq[1],k,ist,i),1)+' ',alignment=1.0 $
                 ,charsize = !p.charsize*0.5 $
              else xyouts,x,y,'- ',color=1,alignment=1.0

           endif  
           wind,x,y,d(iu,k,ist,i),d(iv,k,ist,i),2,size=size

         endfor  ; ist stations
   

            x1 = xc+ddx2
            y1 = yc-ddy2*0.2

        endfor  ; k np
       endfor  ; i time
        
end          




          
 pro get_labels,dw,nlev,levels,c_labels,ndigit
  d1=max(dw(where(dw ge -1000.)))
  d0=min(dw(where(dw ge -1000.)))
  dd=(d1-d0)/(nlev+1)
  if(dd le 1.0e-4)then begin
   levels=d0
   c_labels=strdigit(d0,ndigit)
  endif else begin
   levels=indgen(nlev)*dd+d0+dd
   c_labels=strarr(nlev)
   for i=0,nlev-1 do begin
    c_labels(i)=strdigit(levels(i),ndigit)
   endfor

 endelse

end
  

  
pro stations,d,p,t,lat,lon,st,iu,iv,t0,p0,itemp=itemp,iq=iq,itiq
;  d(nv,np,nst,nt)

    
    plot,lon,lat,xrange=[min(lon),max(lon)],yrange=[min(lat),max(lat)],/nodata
    itime=min(where(t eq t0))
    ip = min(where(p eq p0))

    nst = n_elements(st)

    xyouts,!X.CRANGE(1),!Y.CRANGE(1)-1.,strdigit(t0,2)+' t  ',align=1.
    xyouts,!X.CRANGE(1),!Y.CRANGE(1)-2.,strdigit(p0,0)+'  p  ',align=1.

; --------- contour drawn first
     if(keyword_set(itemp))then begin
         dw=fltarr(nst)
          for i=0,nst-1 do begin
           dw(i)=d(itiq[0],ip,i,itime)
          endfor
        
         jj1 = where(dw ge -1000, count1)
         if(count1 gt 0)then begin
         temmax = max(dw(jj1))
         temmin = min(dw(jj1))
         endif else goto, jump22  

         xyouts,!X.CRANGE(0),!Y.CRANGE(1)-1.,' T('+ $
          strdigit(temmin,2)+', '+ strdigit(temmax,2)+')' ,align=0.,color=6


         get_labels,dw,6,levels,c_labels,2

        contour,dw,lon,lat,levels=levels,max_value=40,$
         min_value=-100,c_labels=c_labels,/overplot,color=6,/irregular
     endif

jump22:

     if(keyword_set(iq))then begin
         dw=fltarr(nst)
          for i=0,nst-1 do begin
           dw(i)=d(itiq[1],ip,i,itime)
          endfor

         jj1 = where(dw ge -1000, count1)
         if(count1 gt 0)then begin
         temmax = max(dw(jj1))
         temmin = min(dw(jj1))
         endif else goto, jump11  

         xyouts,!X.CRANGE(0),!Y.CRANGE(1)-2.,' q('+ $
          strdigit(temmin,3)+', '+ strdigit(temmax,3)+')' ,align=0.,color=2

         get_labels,dw,6,levels,c_labels,3

        contour,dw,lon,lat,levels=levels,$
    max_value=30,min_value=0.,c_labels=c_labels,/overplot,color=2,/irregular
     endif

 jump11:
; -----------------------------------------------------     
        i=itime
        k=ip

        for ist=0,nst-1 do begin
  	 x=lon(ist)
 	 y=lat(ist)  ;
         y2=lat(ist) 
           if(keyword_Set(itemp) )then begin
              if(abs(d(itiq[0],k,ist,i)) le 1200) then $ 
              xyouts,x,y,' '+strdigit(d(itiq[0],k,ist,i),1),alignment=0.0 $
                 ,charsize = !p.charsize*0.2 $
              else xyouts,x,y,' -',color=1,alignment = 0.0
           endif

           if(keyword_Set(iq))then begin
              if(abs(d(itiq[1],k,ist,i)) le 1200) then $ 
              xyouts,x,y,strdigit(d(itiq[1],k,ist,i),1)+' ',alignment=1.0 $
                 ,charsize = !p.charsize*0.5,color=4 $
              else xyouts,x,y,'- ',color=1,alignment=1.0

           endif  
           wind,x,y2,d(iu,k,ist,i),d(iv,k,ist,i),5,size=2

             
        endfor
        
end          
          

   

pro wdall

 while (!D.window ge 0) do wdelete, !D.window

end


pro ycoord,p,x1,x2,ptop,pbottom

      np=n_elements(p)

      ptop = (fix(p(np-1))/100)*100
      pbottom = (fix(p(0)+99.9)/100) *100


      plot,[x1,x2],[ptop,pbottom],xrange=[x1,x2],yrange=[ptop,pbottom], $
                  /nodata,yticks=1,xticks=0,xstyle=1,ystyle=1,ycharsize=0.1

      for k=0,np-1 do begin
        junk = strtrim(p(k),2)
        junk2 = strpos(junk,'.')        
          xyouts,x1,pbottom-p(k)+ptop,strmid(junk,0,junk2)+'-',alignment = 1.0
			
      endfor
end

; -------------------------------------------
 pro get_date,i2,IOP,t
   i2 = -100

 if(IOP eq '9507')then begin
   print,'which date? e.g., 1  187  0718'
   read,idate
   idate = fix(idate)
   julian = idate
    ; calendar or julian

     if(idate ge 400)then begin  ;calender
       julian = julday(idate/100,idate-(idate/100)*100,1995) $
           -julday(7,17,1995) +198
     endif
 
   if(idate le 30)then begin   ;1-18 number
    julian = idate + 197
   endif
  
    i2 = min(where(t ge julian))

 
 endif

;---------------------------------------
 if(IOP eq 'coare')then begin
   print,'which date? e.g., 1  353  921218'
   read,idate
   idate = long(idate)
   julian = idate
    ; calendar or julian

     if(idate ge 900)then begin  ;calender
       iyear = fix(strmid(strtrim(idate,2),0,2))+1900
       imonth = fix(strmid(strtrim(idate,2),2,2))
       iday = fix(strmid(strtrim(idate,2),4,2))
      
       julian = julday(imonth,iday,iyear) $
           -julday(12,18,1992) +353
     endif
 
   if(idate le 30)then begin   ;1-18 number
    julian = idate + 352
   endif
  
    i2 = min(where(t ge julian))

 
 endif

;------------------------
end

pro show_st,sts,lats,lons
 x1=min(lons)
 x2=max(lons)
 y1=min(lats)
 y2=max(lats)
 n=n_elements(lons)
 
 window,/free,title='locations'
 plot,[x1,x2],[y1,y2],/nodata,yrange=[y1,y2]
 for i=0,n-1 do begin
   xyouts,lons[i],lats[i],' '+ strtrim(i,2)+":" +strtrim(sts(i),2),color=1
  xyouts,lons[i],lats[i],'o',color=5
 endfor

end


pro save_gif,filename
 image = TVRD()
 TVLCT,R1,G1,B1,/get
 write_gif,filename,image,R1,G1,B1
 print,'.....saved the following file ...' + filename
end

pro show_gif
; tempfile=pickfile(title='Select file',filter='j*.gif')
; read_gif,tempfile,image,R1,G1,B1
; TVLCT,R1,G1,B1
; window,/free,title=tempfile
; TV,image
end

;======================================
 pro get_steps,it0,level,nt,np,lcycle
; lcycle =+-1  +-2   cycle through pressure, time

  if(abs(lcycle) eq 1)then begin
     if(lcycle eq 1)then begin
        level=level+1
        if(level gt (np-1))then begin
         level=0
         it0=it0+1
        endif
     endif else begin
        level=level-1
        if(level lt 0)then begin
         level=np-1
         it0=it0-1
        endif
      endelse
   endif else begin
      if(lcycle eq 2)then begin
        it0=it0+1
        if(it0 gt (nt-1))then begin
         it0=0
         level=level+1
        endif
       endif else begin
        it0=it0-1
        if(it0 lt 0)then begin
         it0=nt-1
         level=level-1
         endif
        endelse
   endelse

end
