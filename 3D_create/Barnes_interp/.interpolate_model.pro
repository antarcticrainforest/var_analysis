; This routine interpolates the ECMWF analysis
; to the appropriate vertical levels and lat long positions.

; First some moistiure subroutines from Xie...


pro EC_RH_2_q,rh,p,T,r
;T (K),p(mb),r(kg/kg)
;
Rd=287.04
Rv=461.50
ep=Rd/Rv

ai=23.33086
bi=6111.72784
ci=0.15215

esi=exp(ai-bi/T+ci*alog(T))
ei=rh*esi
ri=ei*ep/(p-ei)


esl=1.003*exp(53.67957-6743.769/T-4.8451*alog(T))
el=rh*esl
rl=el*ep/(p-el)
T0=-23.
T1=0.
if ((T-273.16) ge T1) then begin
  r=rl
endif
if ((T-273.16) le T0) then begin
  r=ri
endif
if ((T-273.16) lt T1 and (T-273.16) gt T0) then begin
  fr=(((T-273.16)-T0)/(T1-T0))^2
;  print,fr
  r=fr*rl+(1-fr)*ri
endif 

end

pro REHUM_W,p,T,r,rh	
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
e=r*p/(r+ep)

rh=e/es

end

pro Td_2_rh,p,Td,T,rh	
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
e=1.003*exp(53.67957-6743.769/Td-4.8451*alog(Td))

rh=e/es

end

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


; Input files ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

; the model upper air and surface variables. 
; Created via concatenation of ECMWF monthly
; files as distributed by ARM. 
; See the directory create_ncdf for details. 

   inputr1='XXXX'
   inputr2='XXXX'
   
; an example sounding file. 
; We just use this to get the pressure levels 
; to interpolate on to.

   inputs='./sonde_25mb.di5'

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

; Positions of stations. 
; Stations must be in anticlockwise order 
; CENTRAL STATION LAST.
 
   
   ;lons =[130.41669,129.8,131.1355,131.7609,131.7651,130.8925]
   ;lats =[-11.40891,-12.4,-13.2287,-12.5858,-11.3081,-12.4239]
   lons = [130.121,131.962,131,962,130.131]
   lats = [-11.348,-11.348,-13.147,-13.147]
   
    ;polyx= [130.121, 131.962, 131.962, 130.121 ]
    ;polyy= [-13.147, -13.147, -11.348, -11.348 ]
   nsts = n_elements(lats)


; OUTPUT files ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
   
; 3d filename

   output3d='XXXX'
   
; 2d files are constructed from these variables
; will automatically suffix with .agrid
; e.g. '../Xie_format/surf_T.agrid'

   dir_out='XXXX'
   filesuffix=['p','rh','T','u','v','z']
   fileprefix = 'surf_'      

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
; Read in model data

print,' read in model data'
      
print,'read ecmwf data from : '
print,inputr1
print,inputr2


        cdfread,inputr1,'longitude',lon,rcode  
        cdfread,inputr1,'latitude',lat,rcode  
        cdfread,inputr1,'time',tr,rcode  
        cdfread,inputr1,'levels',pr,rcode  
        nlonr=n_elements(lon)
        nlatr=n_elements(lat)
        nlevr=n_elements(pr)
        ntr=n_elements(tr)
	
	
	; make a matrix of the lat lons.
        lonr=fltarr(nlonr,nlatr)
        latr=fltarr(nlonr,nlatr)
        for i = 0,nlonr-1 do begin
        for j = 0,nlatr-1 do begin
           lonr(i,j) = lon(i)
           latr(i,j) = lat(j)
        endfor
        endfor
	
	
;xie --
        nvar2d=4
	nvar_sfc=6
         ruc=fltarr(nlonr,nlatr,nlevr,nvar2d,ntr)
         ruc_2d=fltarr(nlonr,nlatr,nvar_sfc,ntr)
;xie ++
	 rhxx=fltarr(nlonr,nlatr,nlevr)

         cdfread,inputr2,'spsfc',psr,rcode
	 cdfread,inputr2,'no2dsfc',tdsr,rcode
	 cdfread,inputr2,'no2tsfc',tsr,rcode
	 cdfread,inputr2,'no10usfc',usr,rcode
	 cdfread,inputr2,'no10vsfc',vsr,rcode
	 cdfread,inputr2,'zsfc',zsr,rcode
	 ;convert geoopotential to height
	 zsr = zsr/9.8
	 
	 Td_2_rh, psr,tdsr,tsr,rhsr
          for ilat =0,nlatr-1 do begin
              for ilon = 0,nlonr-1 do begin
                  for i = 0,ntr-1 do begin
                      ruc_2d(ilon,ilat,0,i)=psr (ilon,ilat,i)/100.
		      ruc_2d(ilon,ilat,1,i)=rhsr(ilon,ilat,i)
		      ruc_2d(ilon,ilat,2,i)=tsr(ilon,ilat,i)-273.15
		      ruc_2d(ilon,ilat,3,i)=usr(ilon,ilat,i)
		      ruc_2d(ilon,ilat,4,i)=vsr(ilon,ilat,i)
		      ruc_2d(ilon,ilat,5,i)=zsr(ilon,ilat,i)
                  endfor
              endfor
          endfor
        var_name2d=['tprs','rprs','uprs','vprs']
        for iv = 0, nvar2d-1 do begin
          cdfread,inputr1,var_name2d(iv),var_tmp,rcode  
          for ilat =0,nlatr-1 do begin
              for ilon = 0,nlonr-1 do begin
                  for i = 0,ntr-1 do begin
                      ruc(ilon,ilat,*,iv,i)=var_tmp (ilon,ilat,*,i)
                  endfor
              endfor
          endfor
      endfor
;
; xie ++  modifications needed for EC  RH field ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;  T >0 (C)  relative to water 
;  T <-23 (C) relative to ice
;  T in between  mixed phase 

   for i = 0, ntr-1 do begin
     for ilon = 0,nlonr-1 do begin
     for ilat = 0,nlatr-1 do begin
     for ilev = 0,nlevr-1 do begin

        tsx=ruc(ilon,ilat,ilev,0,i)
        psx=pr(ilev)
        rhsx=ruc(ilon,ilat,ilev,1,i)/100.
    	EC_RH_2_q,rhsx,psx,Tsx,rsx
    	REHUM_W,psx,Tsx,rsx,rhsx
        rhxx(ilon,ilat,ilev)	=  rhsx 
     endfor
     endfor
     endfor
     ruc(*,*,*,1,i) = rhxx(*,*,*)
   endfor
   


; MS - don't really know what this is for... ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


     xie_uv = 1;
     if (Xie_uv gt 0.5) then begin
       for ilon=0,nlonr-1 do begin
        for ilat=0,nlatr-1 do begin         

        ;ilon=3
        ;ilat=11
            ur = reform(ruc(ilon,ilat,*,2,*))
            vr = reform(ruc(ilon,ilat,*,3,*))

         ; longitude w-e, latitude, south-north 
         ip=( (ilon+1) < (nlonr-1) )
         jp=( (ilat+1) < (nlatr-1) )

         im=( (ilon-1) > 0 )
         jm=( (ilat-1) > 0 )

         indlon = [ip,ilon,im,ilon]
         indlat = [ilat,jp,ilat,jm]

         convert_ruc,ur,vr,ur2,vr2,$
               lonr(indlon,indlat),latr(indlon,indlat)


            ruc(ilon,ilat,*,2,*) = ur2(*,*)
            ruc(ilon,ilat,*,3,*) = vr2(*,*)

         endfor
       endfor    
      endif
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
; Ok, we now have all the model data in the arrays ruc and ruc2d. 
; We can begin interpolating.

; Read in example sunding to give pressure levels required.
ipt_vht,inputs,instruments,nvs,nps,nsts,nts,vs,ps,sts,ts,ds





; Number of variables in upper air file   
   nvr_f = 4

; number of surface variables
   nvr_f2 = 6

; Set up some arrays
      dr_f=fltarr(nvr_f,nps,nsts,ntr)
      dr_f_2d=fltarr(nvr_f2,nsts,ntr)
      tr_2d = fltarr(nsts,ntr)

; Main interpolation loop for 3D data. Loop through all times (i,ntr), stations (ist,nsts)
; Variables (m,nvr_f) and pressure levels (j,nps).

     for i=0,ntr-1 do begin
       print,i + 1
        for ist=0,nsts-1 do begin
           for m=0,nvr_f-1 do begin
             for j=0,nps-1 do begin
               xtime=tr(i)
               xlon=lons(ist)
               xlat=lats(ist)
               xp=ps(j)


    	      ; Calculate x(j-1) and x(j) so we can interpolate between
              windowt,ntr,tr,xtime,it1,it2
              windowp,nlevr,pr,xp,ip1,ip2
              windowxy,nlonr,nlatr,lonr,latr,xlon,xlat $
                      ,nwd,iwd,jwd,dwd,100.0
		      
		      
	          ; This is where the magic happens.
		  ; interpolation is linear in pressure
		  ; Barnes scheme (L=50km) in horizontal.


                  dw=fltarr(nwd)
                  dw1=fltarr(nwd)
                  dw2=fltarr(nwd)
                  dwp1=fltarr(nwd)
                  dwp2=fltarr(nwd)

                  for k=0,nwd-1 do begin
                     dw1(k)=ruc(iwd(k),jwd(k),ip1,m,it1)
                     dw2(k)=ruc(iwd(k),jwd(k),ip1,m,it2)
                  endfor
                  itp_2p2d,tr(it1),dw1,tr(it2),dw2,xtime,dwp1
                  
                  for k=0,nwd-1 do begin
                     dw1(k)=ruc(iwd(k),jwd(k),ip2,m,it1)
                     dw2(k)=ruc(iwd(k),jwd(k),ip2,m,it2)
                  endfor
                  itp_2p2d,tr(it1),dw1,tr(it2),dw2,xtime,dwp2

                  itp_2p2d,pr(ip1),dwp1,pr(ip2),dwp2,xp,dw

                  barns1,nwd,dwd,dw,50.0,wd
                 
              dr_f(m,j,ist,i)=wd
           endfor
           endfor
	   ; End loop over levels and variables

    	   ; Begin loop over surface variables
           for m=0,nvr_f2-1 do begin
              xtime=tr(i)
              xlon=lons(ist)
              xlat=lats(ist)
              windowt,ntr,tr,xtime,it1,it2
              windowxy,nlonr,nlatr,lonr,latr,xlon,xlat $
                      ,nwd,iwd,jwd,dwd,100.0
                  dw=fltarr(nwd)
                  dw1=fltarr(nwd)
                  dw2=fltarr(nwd)

              for k=0,nwd-1 do begin
                 dw1(k)=ruc_2d(iwd(k),jwd(k),m,it1)
                 dw2(k)=ruc_2d(iwd(k),jwd(k),m,it2)
              endfor
              itp_2p2d,tr(it1),dw1,tr(it2),dw2,xtime,dw
	   
	      ; No idea wheter this has an effect
              barns1,nwd,dwd,dw,50.0,wd
              dr_f_2d(m,ist,i)=wd
	      tr_2d(ist,i) = tr(i)
	   endfor

        endfor
      endfor


      

      ; Do we want to make a Xie file?
      print,'output ? y/n'
      c1='y'
      ;read,c1
      
      ; Make the Xie file
      if(c1 eq 'y')then begin
      
        vr_f=['temp(K)','rh(%)','u','v']
        vr_f2=['ps(mb)','rh(%)','T(C)','u(m/s)','v(m/s)','z(m)']
	stf=['F1','F2','F3','F4','F5','FC']


	
        c = 'ECMWF'
        print,'outputfile ruc to interpolated grids 2-d'
	
	; 2D: Each variable in a separate file
	for k=0,nvr_f2 - 1 do begin
   	   output2d =dir_out+fileprefix+filesuffix[k]+'.agrid'
           print,output2d
	   
	   opt_arr = fltarr(2,nsts,ntr)
	   opt_arr(0,*,*) = tr_2d(*,*)
	   opt_arr(1,*,*) = dr_f_2d(k,*,*)
           opt_ht,output2d,c,2,nsts,ntr,['t(days)',vr_f2(k)],sts,lons,lats,tr,opt_arr
        endfor

        print,'outputfile ruc to interpolated grids 3-d'
        print,output3d
        opt_vht,output3d,c,nvr_f,nps,nsts,ntr,vr_f,ps,stf,tr,dr_f
      endif 




end



