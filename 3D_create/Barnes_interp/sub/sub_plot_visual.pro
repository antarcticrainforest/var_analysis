     pro bound,dw,xmin1,xmax1,da
         if(max(dw) le -9990.)then begin
           print,'all missing'
           goto,jump1
         endif
         xmin1=min(dw(where(dw gt -9990.0)))-da
         xmax1=max(dw(where(dw gt -9990.0)))+da

         if(xmax1 eq xmin1)then begin
          xmax1=xmax1+1
          xmin1=xmin1-1
         endif
jump1:
end

     pro plotloc,lonf,latf,xmin,xmax,ymin,ymax,lon,lat,stn

         plot,lonf,latf,/ynozero,xrange=[xmin,xmax],$
         yrange=[ymin,ymax],xtitle='lon',ytitle='lat',psym=7,color=1 
         oplot,lon,lat,psym=1,color=2
         
 end

