 pro short_name1,strhead1,strh1,unit1

    nv=n_elements(strhead1)
    strh1=strhead1
    unit1=strh1

    for ii=0,nv-1 do begin
      jc=strtrim(strhead1[ii],2)
      j1=strpos(jc,'(',2)
      j2=strlen(jc)-j1-2
      if(j1 ge 0)then begin
       unit1[ii]=strmid(jc,j1+1,j2)
      endif

       cc=strhead1[ii]

      CASE strtrim(strhead1[ii],2) OF

      'Prec(mm/hour)':       cc='prec'
      'LH_(upward_W/m2)':    cc='lh'
      'SH_(upward_W/m2)':    cc='sh'
      'Area_Mean_Ps(mb)':    cc='psm'
      'Central_Facility_Ps(mb)': cc='psc'
      'Ts_Air(C)':           cc='ts'  
      'Tg_Soil(C)':          cc='tg'
      'Sfc_Air_RH(%)':       cc='rh'
      'Srf_wind_speed(m/s)': cc='vspd'
      'us_wind_(m/s)':       cc='us'
      'vs_wind_(m/s)':        cc='vs'
      'Srf_Net_Dn_Rad(W/m2)':cc='rads'
      'TOA_LW_Up(W/m2)':     cc='lwt'
      'TOA_SW_Dn(W/m2)':     cc='swt'
      'TOA_Ins(W/m2)':       cc='tins'
      'GOES_Lowcld(%)':      cc='lowcld'
      'GOES_Midcld(%)':      cc='midcld'
      'GOES_Hghcld(%)':      cc='hghcld'
      'GOES_Totcld(%)':      cc='totcld'
      'Cld_Thickness(km)':   cc='clddz'
      'Cld_Top_ht(km)':      cc='cldtz'
      'MWR_Cld_liquid(cm)':  cc='cldw'
      'd(Column_H2O)/dt_(mm/hour)': cc='dcqdt'
      'Column_H2O_Advection_(mm/hour)': cc='cqadv' 
      'Srf_Evaporation_(mm/hour)': cc='evp'
      'd(Column_Dry_Static_Energy)/dt_(W/m2)': cc='dcsdt'
      'Column_Dry_Static_Energy_Advection_(W/m2)': cc='cadvs'
      'Column_Radiative_Heating_(W/m2)': cc='crad'
      'Column_Latent_heating_(W/m2)': cc='clh' 
      'omega_surface_(mb/hr)': cc='omegas'
      'qs_surface_(kg/kg)': cc='qs'
      's_surface_(K)':     cc='ss'
      'MWR_precip_water_(cm)': cc='prew' 
      'Siros_Srf_LWUP_(W/m2)': cc='srflwup'
      'Siros_Srf_LWDN_(W/m2)': cc='srflwdn'
      'Siros_Srf_SWUP_(W/m2)': cc='srfswup'
      'Siros_Srf_SWDN_(W/m2)': cc='srfswdn'
     
       else:
     ENDCASE

     strh1[ii]=cc
 endfor

end

;---------------------------

 pro short_name2,strhead2,strh2,unit2

    nv=n_elements(strhead2)
    strh2=strhead2
    unit2=strh2

    for ii=0,nv-1 do begin
      jc=strtrim(strhead2[ii],2)
      j1=strpos(jc,'(',2)
      j2=strlen(jc)-j1-2
      if(j1 ge 0)then begin
       unit2[ii]=strmid(jc,j1+1,j2)
      endif

       cc=strhead2[ii]

      CASE strtrim(strhead2[ii],2) OF

      'Temp_(K)':                cc='T'
      'H2O_Mixing_Ratio_(g/kg)': cc='q'
      'u_wind_(m/s)':            cc='u'
      'v_wind_(m/s)':            cc='v'
      'omega_(mb/hour)':         cc='omega'
      'Wind_Div_(1/s)':          cc='div'
      'Horizontal_Temp__Advec_(K/hour)': cc='hadvT'
      'Vertical_T_Advec(K/hour)':        cc='vadvT'
      'Horizontal_q_Advec_(g/kg/hour)':  cc='hadvq'
      'Vertical_q_Advec(g/kg/hour)':     cc='vadvq'
      's(Dry_Static_Energy)(K)':         cc='s'
      'Horizontal_s_Advec_(K/hour)':     cc='hadvs'
      'Vertical_s_Advec(K/hour)':        cc='vadvs'
      'ds/dt(K/hour)':                   cc='dsdt'
      'DT/dt(K/hour)':                   cc='dTdt'
      'dq/dt_(g/kg/hour)':               cc='dqdt'
      'Q1_(k/hour)':                     cc='Q1'
      'Q2_(K/hour)':                     cc='Q2'
       else:
     ENDCASE

     strh2[ii]=cc
 endfor

end
     
    
