
; =================================
 pro XTOW,X,w,b,wb,bb,REVERSE=reverse
  COMMON SHARED,NV,NH,MV,SIGMA2,A1,A2,A3,A4,XX,ZZ
;  Common Shared

  if(keyword_Set(reverse))then begin
   x=fltarr(nh*nv + nh + mv*nh + mv)
  
   x[0:nh*nv-1]=reform(w,nh*nv)
   x[nh*nv:nh*(nv+1)-1] = b
   x[nh*(nv+1):nh*(nv+1+mv)-1] = reform(wb,mv*nh)
   x[nh*(nv+1+mv):*] = bb

  endif else begin

   w  =reform(x[0:nh*nv-1],nh,nv)
   b  =       x[nh*nv:nh*(nv+1)-1]
   wb =reform(x[nh*(nv+1):nh*(nv+1+mv)-1],mv,nh)
   bb =       x[nh*(nv+1+mv):*]

  endelse

 end


;============================================

FUNCTION NEURAL,X
COMMON SHARED
 ; COMMON SHARED,NV,NH,MV,SIGMA2,A1,A2,A3,A4,XX,ZZ

 ;x=fltarr(nh*nv + nh + mv*nh + mv)
 
 ;data and nh,nv,mv,a1-a4 need to come in from common block

  XTOW,X,w,b,wb,bb

  ;yy=fltarr(nt,nh)
  yy= transpose(w#transpose(xx) )
  for j=0,nh-1 do yy(*,j)=yy(*,j)+b(j)  
  yy =  tanh(yy) 
   ;help,w,b,xx,yy

   zz1=transpose (wb#transpose(yy))
   for j=0,mv-1 do zz1(*,j)=zz1(*,j)+bb(j)
 
   ;help,zz1
  
  weight2 = fltarr(mv,mv)
  for i=0,mv-1 do begin & $
   weight2(i,i)=sigma2(i) & $
  endfor

  C =  total( ((ZZ1 - ZZ) * (ZZ1-ZZ))#weight2 )

  C2 = a1/2.0 * C + a2*a4^2*total (w^2/(a4^2+w^2) ) + $
       a3*a4^2*total( wb^2/(a4^2+wb^2 ))
  C2=c

; this C2 is to be minimized based on input a1,a2,a3,a4,and sigma2(mv)
; to obtain w(nh,nv),b(nh)  and wb(mv,nh),bb(mv)
;
  Return,C2

 end

;============================================

FUNCTION NEURAL_D,X
COMMON SHARED
 ; COMMON SHARED,NV,NH,MV,SIGMA2,A1,A2,A3,A4,XX,ZZ

 ;x=fltarr(nh*nv + nh + mv*nh + mv)
 
 ;data and nh,nv,mv,a1-a4 need to come in from common block

  XTOW,X,w,b,wb,bb

  weight2 = fltarr(mv,mv)
  for i=0,mv-1 do begin & $
   weight2(i,i)=sigma2(i) & $
  endfor

  ;yy=fltarr(nt,nh)
  yy= transpose(w#transpose(xx) )
  for j=0,nh-1 do yy(*,j)=yy(*,j)+b(j)  
  yy =  tanh(yy) 
   ;help,w,b,xx,yy

   ;zz1(nt,mv)
   zz1=transpose (wb#transpose(yy))
   for j=0,mv-1 do zz1(*,j)=zz1(*,j)+bb(j)
 
; starting to calculate the derivatives
  p_w=fltarr(nh,nv)
  p_b=fltarr(nh)
  p_wb=fltarr(mv,nh)
  p_bb=fltarr(mv)

  for i=0,mv-1 do begin
   for j=0,nh-1 do begin
      temp= (zz1(*,i)-zz(*,i))*yy(*,j)  
      p_wb(i,j)=total( temp*sigma2(i) ) 
       p_wb(i,j)=a1*p_wb(i,j)+a3*(a4^2/(a4^2+wb(i,j)^2))^2 * 2.0*wb(i,j)
   endfor
  endfor

  for i=0,mv-1 do begin
     temp=(zz1(*,i) - zz(*,i))
     p_bb(i) = total(temp)
      p_bb(i)=a1*p_bb(i)
  endfor

  DY=1.0-yy*yy 
  for i=0,nh-1 do begin
   for j=0,nv-1 do begin  
    temp = reform(DY(*,i)*xx(*,j)) 
    temp2 = reform(wb(*,i))
    temp3 = transpose(temp2 # temp)  ;ntxnv
    p_w(i,j) =  total( ((ZZ1 - ZZ) *temp3) #weight2 )
      p_w(i,j)=a1*p_w(i,j) +a2*2.0*(a4^2/(a4^2+w(i,j)^2) )^2 * w(i,j)

   endfor
  endfor

  for i=0,nh-1 do begin
    temp = reform(DY(*,i)) 
    temp2 = reform(wb(*,i))
    temp3 = transpose(temp2 # temp)  ;ntxnv
    p_b(i) =  total( ((ZZ1 - ZZ) *temp3) #weight2 )
      p_b(i)=a1*p_b(i) 
   endfor
 
  XTOW,p_X,p_w,p_b,p_wb,p_bb,/reverse

  return,p_X

  ;help,zz1
  

  ;C =  total( ((ZZ1 - ZZ) * (ZZ1-ZZ))#weight2 )

  ;;C2 = a1/2 * C + a2*a4^2*total ((w/a4)^2/(1.+(w/a4)^2) ) + $
  ;;     a3*a4^2*total( (wb/a4)^2/(1.+(wb/a4)^2 ))
  ;  C2=c

; this C2 is to be minimized based on input a1,a2,a3,a4,and sigma2(mv)
; to obtain w(nh,nv),b(nh)  and wb(mv,nh),bb(mv)
;
  ;Return,C2

 end

;=========================
  pro NEURAL2, X,W,B,WB,BB,Z
    Y=tanh(W#X+B)
    Z=WB#Y+BB
  END
