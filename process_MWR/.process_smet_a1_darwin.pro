;-------------------------------------------------------------------------------
;
; Program to read and average mwr data for the ARM sites
;
; Input: mwrlos files, 20 second time resolution, missing data not indicated
;        properly
;
; Output: 1 hour averages with missing value=-9999.
;         Standard deviation, number of values in average, and number of times
;         with strange data are also indicated
;
;-------------------------------------------------------------------------------
;
;
;@/Users/mss/SCM_forcing_ensemble/process_data/process_MWR/read_ncdf
;@/Users/mss/SCM_forcing_ensemble/process_data/process_MWR/is_ncdf
;@/Users/mss/SCM_forcing_ensemble/process_data/process_MWR/outlier_rmean

@WRKDIR/process_MWR/read_ncdf
@WRKDIR/process_MWR/is_ncdf
@WRKDIR/process_MWR/outlier_rmean
@WRKDIR/process_MWR/is_leap
;-------------------------------------------------------------
; options

; some basic paths
  path_prefix='WRKDIR'

; set path of working directory
  workdir=path_prefix+'/process_MWR/temp/'

; set path of input files
  data_path='INPUT'

; set file identifier
  file_id='ID'

; set output directory
  out_dir='OUTPUT'
  plot_dir=out_dir+'Plot/'

; set base times
;  ini_time=julday(10,1,2004,0,0,0)-0.5
  ini_time=XXX
; set years, months,days
  years_int=XXX
  years_str=XXX
  months=XXX
  first=XXX
  last=XXX

; set location (C1 or C2 or C3)
  location='darwin'
  loc_code='C3'

;----------------------------------------------------------------


spawn,'rm -f '+workdir+'*.*'

ref_time_jul=julday(1,1,1970,0,0,0)-0.5
ref_time=long(ini_time-ref_time_jul)*86400

; set missing value identifier
misval=-9999.


; set up plotting stuff

!P.MULTI=[0,1,2]
!P.FONT=0
!X.STYLE=1
!Y.STYLE=1
LOADCT, 0


; define arrays with data for 1 month + last data of previous month
; Note: the latter will only work from second months to process onwards
var_all=fltarr(10,86400*32)
time_all=fltarr(86400*32)
qc_all=strarr(6,86400*32)

; define output arrays
time_out=intarr(744)
vars_out=fltarr(744)

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
	      
	   ;ELSE: print, 'month selection error'
	   ;ENDCASE
	   fd = first[nm]
     ld = last[nm]
     ndays = ld - fd + 1
	   
                ; define variables to read from files
		readvars=['base_time','time_offset','org_precip_rate_mean',$
			'temp_mean','rh_mean',$
			'wspd_vec_mean','wdir_vec_mean','atmos_pressure']



		; copy all files for that month to working directory
    for ndd=fd,ld do begin
      fyear=string(years_str(nm))+string(months(nm))+string(ndd,format='(I2.2)')
		  com='cp '+data_path+'*'+file_id+'*'+loc_code+'.*.'+fyear+'*.*.cdf '+workdir
		  spawn, com
    endfor
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
			
                        
			; read netcdf file
			out=read_ncdf(file_in,data,variable_list=readvars,quiet='quiet')

			; unscramble the information from netcdf read
			nvar=n_tags(data)
			names=tag_names(data)

			; time information
			base_time1=data.(0)
			time_offset1=data.(1)

			; find dimensions for the variable array from this file
			nvar=nvar-2
			ntimes1=n_elements(time_offset1)
			var_orig1=fltarr(nvar,ntimes1)

			; read variables
			for nv=0,nvar-1 do begin
				var_orig1[nv,0:ntimes1-1]=data.(nv+2)
			endfor

			; convert wind speed and direction to u and v

			for nt=0,ntimes1-1 do begin
				ws=var_orig1[3,nt]
				wd=var_orig1[4,nt]
				wdr=2*!PI*wd/360.
				u=-ws*SIN(wdr)
				v=-ws*COS(wdr)
				;if(wd ge 180.) then u=-1.*u
				;if((wd ge 90.) and (wd lt 270.)) then v=-1.*v
				var_orig1[3,nt]=u
				var_orig1[4,nt]=v
			endfor

			; calculate universal time and fill full arrays

			time_all(ntimes:ntimes+ntimes1-1)=LONG64(base_time1+time_offset1)
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

			; set up intervals for testing
			nto=0
			flag=0
			time_low=time_out[nto]-1800
			time_upp=time_out[nto]+1800

			;loop over all times
			FOR nt=0L,ntimes-1 DO BEGIN

				;remove negative and small precipitation (see instrument doc)
				IF(readvars[nv+2] EQ 'precip_mean') THEN BEGIN
					if(var_all[nv,nt] le 0.1) then var_all[nv,nt]=0.
				ENDIF

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
						; start averaging
						IF ((N_ELEMENTS(val) GT 1) AND (flagnan EQ 0)) THEN BEGIN
						
							mean_out[nv,nto]=MEAN(val)
							IF(FINITE(mean_out[nv,nto]) EQ 0) THEN mean_out[nv,nto]=misval
							stdv_out[nv,nto]=STDDEV(val)
							IF(FINITE(stdv_out[nv,nto]) EQ 0) THEN stdv_out[nv,nto]=misval
							nval_out[nv,nto]=N_ELEMENTS(val)
						ENDIF
						flag=0
						nto=min([nto+1,ntimesout-1])
						time_low=time_out[nto]-1800
						time_upp=time_out[nto]+1800
					ENDIF
				ENDIF
				IF((time_all[nt] GT time_low) AND (time_all[nt] LE time_upp)) THEN BEGIN
					IF (flag EQ 0) THEN	BEGIN
						val=var_all[nv,nt]
					ENDIF
					IF (flag EQ 1) THEN BEGIN
						val=[val,var_all[nv,nt]]
					ENDIF
					flag=1
				ENDIF
			ENDFOR
		ENDFOR

		;dummy = LABEL_DATE(DATE_FORMAT = '%D/%H/%I/%S')

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

			;set yrange depending on variable
			IF(readvars[nv+2] EQ 'atmos_pressure') THEN yr=[980.,1040.]
			IF(readvars[nv+2] EQ 'temp_mean') THEN yr=[20.,40.]
			IF(readvars[nv+2] EQ 'relh_mean') THEN yr=[40.,105.]
			IF(readvars[nv+2] EQ 'precip_mean') THEN yr=[0.,30.]
			IF(readvars[nv+2] EQ 'wind1_spd_vec_avg') THEN begin
				yr=[-10.,10.]
				readvars[nv+2]='wind1_u'
			endif
			IF(readvars[nv+2] EQ 'wind1_dir_vec_avg') THEN begin
				 yr=[-10.,10.]
				 readvars[nv+2]='wind1_v'
			endif

			; set up plotting device
			plotfile=plot_dir+'MWR_'+file_id+'_'+readvars[nv+2]+'_'+fyear+'_'+loc_code+'.jpg'
			SET_PLOT, 'Z'
			device, set_resolution=[640,480]
			;device, set_colors=256
			device, z_buffering=0

			; plot data before averaging

			plot,time_all(0:ntimes-1),var_all(nv,0:ntimes-1),MIN_VALUE=-30.,$
				title='Original '+readvars[nv+2]+' '+fyear+' '+location, yrange=yr,$
				xrange=xr, xticks=xt, xtickname=xlab,$
				background=255, color=0

			; cap averaged RH to 100 %
			IF(readvars[nv+2] EQ 'relh_mean') THEN $
				mean_out[nv,0:ntimesout-1]=mean_out[nv,0:ntimesout-1]<100.
			

			plot,time_out,mean_out(nv,0:ntimesout-1),MIN_VALUE=-30.,$
				title='Hourly averaged '+readvars[nv+2]+' '+fyear+' '+location,$
				xrange=xr, xticks=xt, xtickname=xlab,$
				yrange=yr, background=255, color=0

			; capture image and write into jpeg file

			image=tvrd()
			device,/close
			write_jpeg, plotfile,image



			; write results into output file
			; define file
			fileoutv=out_dir+'twp_smet_'+names(nv+2)+'_1h_'+fyear+'_'+loc_code+'.asc'

		    OPENW, 1, fileoutv

			;define header format
		    form='(a10,6x,a4,3x,a2,3x,a2,3x,a2,7x,a4,7x,a4,4x,a4,2x,a6)'

			;write header
			PRINTF, 1, '#     TIME','YYYY','MM','DD','HH','MEAN','STDV',$
				'NVAL', FORMAT=form

			;define data format
		    form='(i15,4(1x,i4),2(1x,f10.4),1(1x,f7.0))'

			;write
			FOR nt=0,ntimesout-1 DO BEGIN
				CALDAT, time_out_jul[nt], month, day, year, hour,m,s
				IF (m>30) THEN hour=hour+1
				PRINTF, 1, time_out[nt],year,month,day,hour,$
					mean_out[nv,nt],stdv_out[nv,nt],nval_out[nv,nt],$
					FORMAT=form
			ENDFOR

			;close file
			CLOSE, 1
		ENDFOR

		; add one month in seconds to reference time
		ref_time=ref_time+ndays*86400

		;delete all but last file from the current months
    SPAWN, 'mv '+file_list[nfiles-1]+' '+path_prefix+'process_MWR/'
		SPAWN, 'rm -f *.*'
		SPAWN, 'mv '+path_prefix+'process_MWR/'+file_list[nfiles-1]+' '+file_list[nfiles-1]

	; end loop over months
	endfor


DEVICE, /CLOSE
CLOSE, /ALL

cd, '../'

end
