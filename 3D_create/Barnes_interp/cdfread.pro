; just use 
; cdfread,cdffile,varname,var_arr
; example:
;	cdffile='sgp60wpdnwndsX1.b1.950718.000000.cdf'
;       cdfread,cdffile,'u_wind_low',u_wind_low
;
;
 pro cdfread,infile,vname,vfield,rcode

 Inid=ncdf_open(infile,/nowrite) ;  open for read
 in_varid=ncdf_varid(Inid,vname)
 if(in_varid eq -1)then begin
 print, "Can't find Variable ",vname, ' in inputfile ',infile
 ncdf_close,Inid
 rcode=-1
 return
 endif else begin
   ncdf_varget,Inid,in_varid,vfield
 endelse
 ncdf_close,Inid
 rcode=0
 end
