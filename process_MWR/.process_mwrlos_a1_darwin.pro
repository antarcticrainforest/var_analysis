;-------------------------------------------------------------------------------
;
; Program to read and average mwr data for the ARM sites
;
; Input: mwrlos files, 20 second time resolution, missing data not indicated
;        properly
;
; Output: 1 hour averages with missing value=-9999.,
;         Missing value -8888 indicates wet window
;         Missing value -7777 indicates rain measured with sfcmet
;         that has not been picked up by wet_window flag
;		  Missing value -6666 indicates sunglare problems
;
;-------------------------------------------------------------------------------
;
;
@WRKDIR/process_MWR/read_ncdf
@WRKDIR/process_MWR/is_ncdf
@WRKDIR/process_MWR/is_leap
@WRKDIR/process_MWR/outlier_rmean

;-------------------------------------------------------------------------------
; Options

; some basic paths
  path_prefix='WRKDIR'

 location='Darwin'
 loc_code='C3'

; set path of input files
  data_path='INPUT'
  
; set file identifier
  file_id='ID'

; set path for darwin surf meteorolgy files
  hourly_path='INPUT'

; set path for ascii output files  
  ascii_path='OUTPUT'
  OUTPUT=XXX
; define netcdf out file
  seas=XXX
  fileoutncdf=OUTPUT+'mwrlos_6h_'+seas+'_'+loc_code+'_interp3.nc'


; set years and months required
  years_int=XXX
  years_str=XXX
  months=XXX
  first=XXX
  last=XXX
; set path of working directory
  workdir=OUTPUT+'/process_MWR/temp/'

; set initial times  ***must match reference time in smet file***
  ini_time=XXX

; set the average time periods
  t_avg = 6

;-------------------------------------------------------------------------------

spawn,'rm '+workdir+'*.* 2>/dev/null'

ref_time_jul=julday(1,1,1970,0,0,0)-0.5
ref_time=long(ini_time-ref_time_jul)*86400
ref_time_init = ref_time

; set missing value identifier
misval=-9999.

; setup some general plotting stuff

!P.MULTI=[0,1,2]
!P.FONT=0
!X.STYLE=1
!Y.STYLE=1
LOADCT, 0
vliq='mwr_vli'
vvap='mwr_vva'
vwet_window='mwr_vwet'
vprecip='mwr_vprecip'
; define variables to read from files
readvars=['base_time','time_offset',vwet_window,vvap,vliq]


; define arrays with data for 1 month + last data of previous month
; Note: the latter will only work from second months to process onwards
var_all=fltarr(10,86400*32)
time_all=fltarr(86400*32)
wet_all=fltarr(86400*32)

; define output arrays
time_out=intarr(744)
vars_out=fltarr(744)

; define full arrays
time_full = dblarr(1000)
vap_full = dblarr(1000)
liq_full = dblarr(1000)
n_ipt_full = intarr(1000)
bad_info_full = intarr(1000,3)

avg_vap =0D
avg_liq = 0D
num_nonzero = 0L

; set some initial values
time_ind = 0


; find number of years and months to process
nmonths=n_elements(months)-1


	; start loop over months
	for nm=0,nmonths do begin
	

	   ; set number of days per month for processing
	   ; watch out for February
	   
	   ;CASE months(nm) OF
	   ;
	   ;   '01': ndays=31
	   ;   '02': IF(is_leap(years_str[nm]) EQ 1) THEN ndays=29 ELSE ndays=28
	   ;   '03': ndays=31
	   ;   '04': ndays=30
	   ;   '05': ndays=31
	   ;   '06': ndays=30
	   ;   '07': ndays=31
	   ;   '08': ndays=31
	   ;   '09': ndays=30
	   ;   '10': ndays=31
	   ;   '11': ndays=30
	   ;   '12': ndays=31
	   ;
	   ;ELSE: print, 'month selection error'
	   ;ENDCASE

	   fd = first[nm]
     ld = last[nm]
     ndays = ld - fd + 1
    
     ; copy all files for that month to working directory
    for ndd=fd,ld do begin
      fyear=string(years_str(nm))+string(months(nm))+string(ndd,format='(I2.2)')
		  com='cp '+data_path+'*'+file_id+'*'+loc_code+'.*.'+fyear+'*.*.cdf '+workdir
		  ;print, com
      spawn, com
    endfor
    ;exit

		
		;;; Names of inout and output files ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
                ; generate filename for precipitation file to read

                fileprec=hourly_path+'twp_smet_'+vprecip+'_1h_'+fyear+'_'+loc_code+'.asc'

    		;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
		

		; make a string array containing all files in the working directory
		; and find out how many there are

		cd, workdir
		to_find='*'+file_id+'*'
		file_list=file_search(to_find)
    	        nfiles=n_elements(file_list)
    	        file_list=file_list(sort(file_list))

		; initialize overall counter of time
		ntimes=0

		;start loop over files
		for nf=0,nfiles-1 do begin

			; set input file
			file_in=file_list(nf)
			
			;print, file_in

			; read netcdf file
			out=read_ncdf(file_in,data,variable_list=readvars,quiet='quiet')

			; unscramble the information from netcdf read
			nvar=n_tags(data)
			names=tag_names(data)

			; time information
			base_time1=data.(0)
			time_offset1=data.(1)

			; wet window information
			wet_orig=fltarr(86400)
			wet_orig=data.(2)


			; find dimensions for the variable array from this file
			nvar=nvar-3
			ntimes1=n_elements(time_offset1)
			var_orig1=fltarr(nvar,ntimes1)

			; read variables
			for nv=0,nvar-1 do begin
				var_orig1(nv,0:ntimes1-1)=data.(nv+3)
			endfor

			; calculate universal time and fill full arrays

			time_all(ntimes:ntimes+ntimes1-1)=LONG64(base_time1+time_offset1)
			wet_all[ntimes:ntimes+ntimes1-1]=wet_orig[0:ntimes1-1]
			for nv=0,nvar-1 do begin
				var_all(nv,ntimes:ntimes+ntimes1-1)=var_orig1(nv,0:ntimes1-1)
			endfor

			ntimes=ntimes+ntimes1

		;end loop over files
		endfor

		; make hourly averages by looping over all data
		; check whether data is in required time interval
		; make a vector for data within time interval
		; and average

		; Note: The procedure assumes that data is in
		; ascending time order !!!

		; make a vector of the expected output times
		; as well as all outputs
		; Outputs are:
		;		Mean
		;		Standard deviation
		;		Number of points that entered in statistics
		; set all resulting values to missing initially

		ntimesout=ndays*24
		time_out=ref_time+LONG(FINDGEN(ntimesout))*3600
		mean_out=fltarr(nvar,ntimesout)
		stdv_out=fltarr(nvar,ntimesout)
		nval_out=fltarr(nvar,ntimesout)
		mean_out[0:nvar-1,0:ntimesout-1]=misval
		stdv_out[0:nvar-1,0:ntimesout-1]=misval
		nval_out[0:nvar-1,0:ntimesout-1]=misval


		; loop over all data, i.e., variables and time
		FOR nv=0,nvar-1 DO BEGIN


                ; define file ascii out ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
		
                fileoutv=ascii_path+'twp_'+file_id+'_'+readvars[nv+3]+'_1h_'+fyear+'_'+loc_code+'.asc'

		;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
			; set up intervals for testing
			nto=0
			flag=0
			wetflag=0
			wetsum=0.
			nval_possible=0
			time_low=time_out[nto]-1800
			time_upp=time_out[nto]+1800

			; set liquid water and precipitable water vapour minimum to zero
			IF(readvars[nv+3] EQ vliq) THEN var_all(nv,0:ntimes-1)=var_all(nv,0:ntimes-1)>0.
    	    	    	IF(readvars[nv+3] EQ vvap) THEN var_all(nv,0:ntimes-1)=var_all(nv,0:ntimes-1)>0.
			
			;loop over all times
			FOR nt=0L,ntimes-1 DO BEGIN
				; do actual processing
				IF(time_all[nt] GT time_upp) THEN BEGIN
					IF (flag EQ 0) THEN BEGIN
						nto=min([nto+1,ntimesout-1])
						time_low=time_out[nto]-1800
						time_upp=time_out[nto]+1800
					ENDIF
					IF (flag EQ 1) THEN BEGIN
						; set flag if any of the values is NaN
						; an hour with NaN will be discarded later
						flagnan=0
						FOR i=0,N_ELEMENTS(val)-1 DO BEGIN
							IF(FINITE(val[i]) EQ 0) THEN flagnan=1
						ENDFOR
						nvals=N_ELEMENTS(val)
						if (wetsum gt nval_possible/2) then wetflag=1
						; start averaging
						IF ((N_ELEMENTS(val) GT 1) AND (flagnan EQ 0) and (wetflag eq 0)) THEN BEGIN
							mean_out[nv,nto]=MEAN(val)
							IF(FINITE(mean_out[nv,nto]) EQ 0) THEN mean_out[nv,nto]=misval
							stdv_out[nv,nto]=STDDEV(val)
							IF(FINITE(stdv_out[nv,nto]) EQ 0) THEN stdv_out[nv,nto]=misval
							nval_out[nv,nto]=N_ELEMENTS(val)
						ENDIF
						if (wetflag eq 1) then begin
							;qc_out[nv,nto]=-8888.
							mean_out[nv,nto]=misval+1111.
							stdv_out[nv,nto]=misval+1111.
							nval_out[nv,nto]=misval+1111.
						endif
						flag=0
						wetflag=0
						wetsum=0.
						nval_possible=0
						nto=min([nto+1,ntimesout-1])
						time_low=time_out[nto]-1800
						time_upp=time_out[nto]+1800
					ENDIF
				ENDIF
				IF((time_all[nt] GT time_low) AND (time_all[nt] LE time_upp)) THEN BEGIN
					nval_possible=nval_possible+1
					wetsum=wetsum+wet_all[nt]
					IF ((flag EQ 0) and (wet_all[nt] eq 0.)) THEN	BEGIN
						val=var_all[nv,nt]
						flag=1
					ENDIF
					IF ((flag EQ 1) and (wet_all[nt] eq 0.)) THEN BEGIN
						val=[val,var_all[nv,nt]]
					ENDIF
				ENDIF
			ENDFOR
		ENDFOR

		; Quality control - Part 2
		; read in hourly precipitation and screen out hours with precipitation



		;define format
	    form='(i15,4(1x,i4),2(1x,f10.4),1(1x,f7.0))'

		; open file
		OPENR, 1, fileprec
		line=strarr(1)
		readf,1,line
		FOR nt=0L,ntimesout-1 DO BEGIN
			; read data
			READF, 1,time_in,x2,x3,x4,x5,precip,x6,x7,FORMAT=form
			; set vapour/liquid to missing for precipitating hours
			IF(time_in EQ time_out[nt]) THEN BEGIN
			 	IF((precip GT 0.01) OR (precip EQ misval)) THEN BEGIN
;			 	IF(precip GT 0.01) THEN BEGIN
					FOR nv=0,nvar-1 DO BEGIN
							mean_out[nv,nt]=misval+2222.
							stdv_out[nv,nt]=misval+2222.
							nval_out[nv,nt]=misval+2222.
					ENDFOR
				ENDIF
			 	IF(precip GT .1) THEN BEGIN
					FOR nv=0,nvar-1 DO BEGIN
						IF(nt+1 LT ntimesout) THEN BEGIN
							mean_out[nv,nt+1]=misval+2222.
							stdv_out[nv,nt+1]=misval+2222.
							nval_out[nv,nt+1]=misval+2222.
						ENDIF
						IF(nt+2 LT ntimesout) THEN BEGIN
							mean_out[nv,nt+2]=misval+2222.
							stdv_out[nv,nt+2]=misval+2222.
							nval_out[nv,nt+2]=misval+2222.
						ENDIF
					ENDFOR
				ENDIF
			 	IF(precip GT .3) THEN BEGIN
					FOR nv=0,nvar-1 DO BEGIN
						IF(nt+3 LT ntimesout) THEN BEGIN
							mean_out[nv,nt+3]=misval+2222.
							stdv_out[nv,nt+3]=misval+2222.
							nval_out[nv,nt+3]=misval+2222.
						ENDIF
						IF(nt+4 LT ntimesout) THEN BEGIN
							mean_out[nv,nt+4]=misval+2222.
							stdv_out[nv,nt+4]=misval+2222.
							nval_out[nv,nt+4]=misval+2222.
						ENDIF
						IF(nt+5 LT ntimesout) THEN BEGIN
							mean_out[nv,nt+5]=misval+2222.
							stdv_out[nv,nt+5]=misval+2222.
							nval_out[nv,nt+5]=misval+2222.
						ENDIF
					ENDFOR
				ENDIF
			ENDIF
		ENDFOR
		; close file
		CLOSE,1

		;generate output times as Julian Day (a la IDL)
		time_out_jul=0.5+double(time_out/86400.)+ref_time_jul

		; set up xaxis for plotting
		xr=[ref_time,ref_time+ndays*86400]
		xt=ndays
		xlab=strtrim(string(findgen(xt)+1,format='(i2)'),2)
		for nd=0,ndays-1 do begin
			if((nd mod 5) ne 0) then xlab[nd]=' '
		endfor
		xlab=[xlab,' ']

		;loop over variables for plot/write
		FOR nv=0,nvar-1 DO BEGIN
		    ; write results into output file
		    ;open file
		    OPENW, 1, fileoutv

			;define header format
		    form='(a10,6x,a4,3x,a2,3x,a2,3x,a2,7x,a4,7x,a4,4x,a4)'

			;write header
			PRINTF, 1, '#     TIME','YYYY','MM','DD','HH','MEAN','STDV',$
				'NVAL', FORMAT=form

			;define format
		    form='(I15,4(1x,i4),2(1x,f10.4),1(1x,f7.0))'

			;write
			FOR nt=0L,ntimesout-1 DO BEGIN
				CALDAT, time_out_jul[nt], month, day, year, hour,m,s
				IF (m>30) THEN hour=hour+1

				; remove sunglare problems for Nauru in March and September
				; by rejecting 1 UTC values
				if ((location eq 'nauru') and ((month eq 3) or (month eq 9)) $
					and (hour eq 1)) then begin
						mean_out[nv,nt]=misval+3333.
						stdv_out[nv,nt]=misval+3333.
						nval_out[nv,nt]=misval+3333.
				endif

				; remove sunglare problems for Manus in March and September
				; by rejecting 1 UTC values
				if ((location eq 'manus') and ((month eq 3) or (month eq 9)) $
					and (hour eq 2)) then begin
						mean_out[nv,nt]=misval+3333.
						stdv_out[nv,nt]=misval+3333.
						nval_out[nv,nt]=misval+3333.
				endif

				PRINTF, 1, time_out[nt],year,month,day,hour,$
					mean_out[nv,nt],stdv_out[nv,nt],nval_out[nv,nt],$
					FORMAT=form
			ENDFOR

			;close file
			CLOSE, 1

			; set up plotting device
			plotfile=OUTPuT+'Plot/MWR_'+file_id+'_'+readvars[nv+3]+'_'+fyear+'_'+loc_code+'.jpg'
			SET_PLOT, 'Z'
			device, set_resolution=[640,480]
			;device, set_colors=256
			device, z_buffering=0

			; plot original data before averaging

			;set yrange depending on variable
			IF(readvars[nv+3] EQ vvap) THEN yr=[0.,10.]
			IF(readvars[nv+3] EQ vliq) THEN yr=[0.,0.5]

			plot,time_all(0:ntimes-1),var_all(nv,0:ntimes-1),MIN_VALUE=-0.1,$
				title=readvars[nv+3]+' '+fyear+' '+location, yrange=yr,$
				xrange=xr, xticks=xt, xtickname=xlab,$
				background=255, color=0

			;plot all variables after averaging
			;set yrange depending on variable
			plot,time_out,mean_out(nv,0:ntimesout-1),MIN_VALUE=-0.1,$
				title='Hourly averaged '+readvars[nv+3]+' '+fyear+' '+location,$
				xrange=xr, xticks=xt, xtickname=xlab,$
				yrange=yr, background=255, color=0

			; capture image and write into jpeg file

			image=tvrd()
			device,/close
			write_jpeg, plotfile,image

		ENDFOR



		;delete all but last file from the current months
		SPAWN, 'mv '+file_list[nfiles-1]+' '+path_prefix+'process_MWR/'
		SPAWN, 'rm *'
		SPAWN, 'mv '+path_prefix+'process_MWR/'+file_list[nfiles-1]+' '+file_list[nfiles-1]

		; make some decent names
		vap = reform(mean_out(0,0:ntimesout-1))
		liq = reform(mean_out(1,0:ntimesout-1))
		FOR nt=0,ntimesout-1 DO BEGIN
		
		   if (liq(nt) GT -1) THEN BEGIN 
		       ;liq(nt) = liq(nt)*10^4 ! convert to g/m^2
		   endif
		ENDFOR

		;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
		; ok, now we want to create 6 hour averages.
		; There is a lot of missing data due to rain. For variational analysis purposes this is a small term
		; so we should be ok with any old hack as a work around.We try two methods.
		;
		; If any hour in the six has data we take an average (even if it is only one data point)
		; 
		; For long periods of no data:
		;
		; 1. dead simple: set to zero - this is a really bad way of doing it. So if the results for this method
		; aren't too bad, we can have confidence that the method doesn't really matter.
		;
		; 2. Exponential decay to the mean over tau = 24 hours, with exponential growth to the given value at the
		; other side.
		;
		; 3. Or, we can use ECMWF. if we can find the data

		
		ntimelong=floor(ntimesout/t_avg)
		
		
		time_out_long=ref_time+LONG(FINDGEN(ntimelong))*t_avg*3600
		nto=0
	
		time_low=time_out_long[nto]-t_avg*1800
		time_upp=time_out_long[nto]+t_avg*1800

		nval_possible = 0
	
		n_arr = ceil(744/t_avg)
		liq_avg = fltarr(n_arr)
		vap_avg = fltarr(n_arr)
		nval = intarr(n_arr)
			FOR nt=0L,ntimesout-1 DO BEGIN

				; do actual processing
				IF(time_out[nt] GT time_upp) THEN BEGIN
				   IF (nto LT ntimelong) THEN BEGIN
					IF (nval_possible EQ 0) THEN BEGIN  	; No data at all for this 6 hour period
						vap_avg[nto] = -9999
						liq_avg[nto] = -9999         
					ENDIF ELSE BEGIN
					    IF (nval[nto] EQ 0 ) THEN BEGIN 	; all data is corrupted by wet instrument
						vap_avg[nto] = 0
					        liq_avg[nto] = 0
					    ENDIF ELSE BEGIN 			; at least some good data is available
					        vap_avg[nto] = vap_avg[nto]/nval[nto]
					        liq_avg[nto] = liq_avg[nto]/nval[nto]
						avg_vap = avg_vap+vap_avg[nto]
						avg_liq = avg_liq+liq_avg[nto]
						num_nonzero = num_nonzero+1			       
					   ENDELSE
					ENDELSE
				   ENDIF
					nto=nto+1
					IF nto GE ntimelong THEN BEGIN
					   BREAK
					ENDIF
					nval_possible = 0
					time_low=time_out_long[nto]-t_avg*1800
					time_upp=time_out_long[nto]+t_avg*1800
					
				ENDIF
				IF((time_out[nt] GT time_low) AND (time_out[nt] LE time_upp)) THEN BEGIN
					nval_possible=nval_possible+1
					IF (vap[nt] GT -1.0 ) THEN BEGIN
					   vap_avg[nto] = vap_avg[nto] + vap[nt]
					   nval[nto] = nval[nto] + 1
					ENDIF   

					IF (liq[nt] GT -1.0 ) THEN BEGIN
					   liq_avg[nto] = liq_avg[nto] + liq[nt]
					ENDIF

				ENDIF
			ENDFOR

		; put stuff into the full arrays
		time_full[time_ind:time_ind+ntimelong-1] = time_out_long[0:ntimelong-1]
		vap_full[time_ind:time_ind+ntimelong-1] = vap_avg[0:ntimelong-1]
		liq_full[time_ind:time_ind+ntimelong-1] = liq_avg[0:ntimelong-1]
		n_ipt_full[time_ind:time_ind+ntimelong-1] = nval[0:ntimelong-1]

		time_ind = time_ind+ntimelong

		
		;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
		; assumes that data is in
		; ascending time order !!!
		; add one month in seconds to reference time
		ref_time=ref_time+ndays*86400

	; end loop over months
	endfor



; fix missing data via exp interpolation?
exp_interp = 1

IF (exp_interp GT 0) THEN BEGIN


    tau = floor(24/t_avg);
    avgtau = floor(24*2/t_avg)
    		
    nt = 0
    WHILE (nt LT time_ind-1) DO BEGIN
		
    	if (n_ipt_full[nt] EQ 0) THEN BEGIN
   
       	    start_ind = nt
		      
    	    WHILE (n_ipt_full[nt] EQ 0 and nt LT time_ind) DO nt = nt+1
	       end_ind = nt-1
	       
	       print, 'data missing at', double(long(double(time_full[start_ind])) - ref_time_init)/86400.00000, double(long(double(time_full[end_ind])) - ref_time_init)/86400.00000
	      
          ; first check if we have missing data at beginning or end
	      if (start_ind EQ 0) THEN BEGIN
	         indicies = where(n_ipt_full[end_ind+1:end_ind+1+avgtau] GT 0, num_nonzero)
		 indicies = indicies + end_ind+1
	         avg_vap2 = total(vap_full[indicies])/num_nonzero
	         avg_liq2 = total(liq_full[indicies])/num_nonzero
		 
 
		 for i = start_ind,end_ind do begin
		    liq_full[i] = (liq_full[end_ind+1]-avg_liq2)*exp((i-end_ind)/float(tau))+avg_liq2
		    vap_full[i] = (vap_full[end_ind+1]-avg_vap2)*exp((i-end_ind)/float(tau))+avg_vap2
		 endfor

	      endif ELSE BEGIN
	         if (end_ind EQ time_ind-1) THEN BEGIN
		 
		 
		    indicies = where(n_ipt_full[start_ind-1-avgtau:start_ind-1] GT 0, num_nonzero)
		    indicies = indicies + start_ind-1-avgtau
		    
	            avg_vap1 = total(vap_full[indicies])/num_nonzero
	            avg_liq1 = total(liq_full[indicies])/num_nonzero
		 
	 
		    for i = start_ind,end_ind do begin
		       liq_full[i] = (liq_full[start_ind-1]-avg_liq1)*exp(-(i-start_ind+1)/float(tau))+avg_liq1
		       vap_full[i] = (vap_full[start_ind-1]-avg_vap1)*exp(-(i-start_ind+1)/float(tau))+avg_vap1
		    endfor
	         endif ELSE BEGIN
		    
		    if (end_ind-start_ind LT tau) THEN BEGIN
	               for i = start_ind,end_ind DO BEGIN
		          vap_full[i] = vap_full[start_ind-1] + (i-start_ind+1)*(vap_full[end_ind+1] - vap_full[start_ind-1])/(end_ind-start_ind+1)
		          liq_full[i] = liq_full[start_ind-1] + (i-start_ind+1)*(liq_full[end_ind+1] - liq_full[start_ind-1])/(end_ind-start_ind+1)
		       endfor
		    endif ELSE BEGIN
		    
		          indicies = where(n_ipt_full[end_ind+1:min([end_ind+1+avgtau,time_ind-1])] GT 0, num_nonzero)
			  indicies = indicies + end_ind+1
	                  
			  avg_vap2 = total(vap_full[indicies])/num_nonzero
	                  avg_liq2 = total(liq_full[indicies])/num_nonzero
		          
		     
         		  indicies = where(n_ipt_full[max([0,start_ind-1-avgtau]):start_ind-1] GT 0, num_nonzero)
			  indicies = indicies + max([0,start_ind-1-avgtau])
			  
	                  avg_vap1 = total(vap_full[indicies])/num_nonzero
	                  avg_liq1 = total(liq_full[indicies])/num_nonzero

	  
			  
		          avg_liq = fltarr(end_ind+1)
		          avg_vap = fltarr(end_ind+1)
		    
		       for i = start_ind,end_ind DO BEGIN
		       
		       
		          avg_liq[i] = avg_liq1 + (avg_liq2-avg_liq1)*(i-start_ind+1)/(end_ind+2-start_ind)
		          avg_vap[i] = avg_vap1 + (avg_vap2-avg_vap1)*(i-start_ind+1)/(end_ind+2-start_ind)
		       
		          vap_full[i] = (vap_full[start_ind-1]-avg_vap[i])*exp(-(i-start_ind+1)/float(tau)) + (vap_full[end_ind+1]-avg_vap[i])*exp((i-end_ind-1)/float(tau))+avg_vap[i]
		          liq_full[i] = (liq_full[start_ind-1]-avg_liq[i])*exp(-(i-start_ind+1)/float(tau)) + (liq_full[end_ind+1]-avg_liq[i])*exp((i-end_ind-1)/float(tau))+avg_liq[i]
		       endfor
		    endelse
		 endelse
    	      endelse
	endif

    	   nt = nt+1
    endwhile
endif
; now we want to create a netcdf file
		fid = NCDF_CREATE(fileoutncdf, /CLOBBER)
		
		timeid=NCDF_DIMDEF(fid,'time',/UNLIMITED)
		
		timevarid=NCDF_VARDEF(fid,'time',[timeid], /FLOAT)
		bdateid=NCDF_VARDEF(fid,'bdate',/LONG)
		be_pwvid=NCDF_VARDEF(fid,'be_pwv',[timeid],/FLOAT)
		be_lwpid=NCDF_VARDEF(fid,'be_lwp',[timeid],/FLOAT)
		nvalid=NCDF_VARDEF(fid,'num_ipt_val',[timeid],/LONG)
		
		
		NCDF_ATTPUT, fid,  timevarid, 'units', 'day since 2004-10-01 00:00:00', /CHAR
		NCDF_ATTPUT, fid,  bdateid, 'units', 'yyyy-mm-dd', /CHAR
				
		NCDF_ATTPUT, fid,  be_pwvid, 'missing_value', -9999, /FLOAT
		NCDF_ATTPUT, fid,  be_pwvid, 'long_name', 'Precipitable water vapour', /CHAR
		NCDF_ATTPUT, fid,  be_pwvid, 'units', 'cm', /CHAR
		
		NCDF_ATTPUT, fid,  be_lwpid, 'missing_value', -9999, /FLOAT
		NCDF_ATTPUT, fid,  be_lwpid, 'units', 'cm', /CHAR
		NCDF_ATTPUT, fid,  be_lwpid, 'long_name', 'Liquid water path', /CHAR
		
		NCDF_ATTPUT, fid,  nvalid, 'long_name', 'number of uncorrupted hours in period', /CHAR
		
		NCDF_CONTROL, fid, /ENDEF
		
		
		
                
		time_nc = double(long(double(time_full[0:time_ind-1])) - ref_time_init)/86400.00000

	
		NCDF_VARPUT, fid, timevarid, time_nc
		NCDF_VARPUT, fid, bdateid, XXbase_timeXX
		NCDF_VARPUT, fid, be_pwvid, vap_full[0:time_ind-1]
		NCDF_VARPUT, fid, be_lwpid, liq_full[0:time_ind-1]
		NCDF_VARPUT, fid, nvalid, n_ipt_full[0:time_ind-1]
		
		
		
		
		NCDF_CLOSE, fid
		


DEVICE, /CLOSE
CLOSE, /ALL

cd, '../'

end
