pro line_fit_xytz2,n,x,y,t,z,dzdx2,dzdy2,dzdxy,dzdx,dzdy,dzdt,z0

  sx4=total(x*x*x*x)
 sx3y=total(x*x*x*y)
sx2y2=total(x*x*y*y)
 sxy3=total(x*y*y*y)
  sy4=total(y*y*y*y)
  sx3=total(x*x*x)
 sx2y=total(x*x*y)
 sxy2=total(x*y*y)
  sy3=total(y*y*y)
  sx2=total(x*x)
  sxy=total(x*y)
  sy2=total(y*y)
   sx=total(x)
   sy=total(y)
 sx2t=total(x*x*t)
 sy2t=total(y*y*t)
 sxyt=total(x*y*t)
  sxt=total(x*t)
  syt=total(y*t)
  st2=total(t*t)
   st=total(t)
 sx2z=total(x*x*z)
 sy2z=total(y*y*z)
 sxyz=total(x*y*z)
  sxz=total(x*z)
  syz=total(y*z)
  stz=total(t*z)
   sz=total(z)

a=[[  sx4, sx2y2, sx3y,  sx3, sx2y, sx2t,  sx2],$
   [sx2y2,   sy4, sxy3, sxy2,  sy3, sy2t,  sy2],$
   [ sx3y,  sxy3,sx2y2, sx2y, sxy2, sxyt,  sxy],$
   [  sx3,  sxy2, sx2y,  sx2,  sxy,  sxt,   sx],$
   [ sx2y,   sy3, sxy2,  sxy,  sy2,  syt,   sy],$
   [ sx2t,  sy2t, sxyt,  sxt,  syt,  st2,   st],$
   [  sx2,   sy2,  sxy,   sx,   sy,   st,    n]]

b= [ sx2z,  sy2z, sxyz,  sxz,  syz,  stz,   sz]

;xie nr_ludcmp,a,index
;xie c=nr_lubksb(a,index,b)

ludc,a,index
c=lusol(a,index,b)

dzdx2=c(0)
dzdy2=c(1)
dzdxy=c(2)
dzdx=c(3)
dzdy=c(4)
dzdt=c(5)
z0=c(6)

end


pro line_fit_xytz1,n,x,y,t,z,dzdx,dzdy,dzdt,z0

  sx2=total(x*x)
  sy2=total(y*y)
  st2=total(t*t)
  sxy=total(x*y)
  syt=total(y*t)
  stx=total(t*x)
   sx=total(x)
   sy=total(y)
   st=total(t)
  sxz=total(x*z)
  syz=total(y*z)
  stz=total(t*z)
   sz=total(z)

a=[[sx2,sxy,stx,sx],$
   [sxy,sy2,syt,sy],$
   [stx,syt,st2,st],$
   [ sx, sy, st, n]]

b= [sxz,syz,stz,sz]

;xie nr_ludcmp,a,index
;xie c=nr_lubksb(a,index,b)

ludc,a,index
c=lusol(a,index,b)

dzdx=c(0)
dzdy=c(1)
dzdt=c(2)
z0=c(3)

end


pro line_fit_xyz,n,x,y,z,dzdx,dzdy,z0

  sx2=total(x*x)
  sy2=total(y*y)
  sxy=total(x*y)
   sx=total(x)
   sy=total(y)
  sxz=total(x*z)
  syz=total(y*z)
   sz=total(z)

a=[[sx2,sxy,sx],$
   [sxy,sy2,sy],$
   [ sx, sy, n]]

b=[sxz,syz,sz]

;xie nr_ludcmp,a,index
;xie c=nr_lubksb(a,index,b)

ludc,a,index
c=lusol(a,index,b)

dzdx=c(0)
dzdy=c(1)
z0=c(2)

end


pro horizontal_field,nst,lon,lat,x,y,f,dzdx,dzdy,divu,divv

x=fltarr(nst)
y=fltarr(nst)
f=fltarr(nst)
pi=3.1415926
a=6.371E+6
omega=7.292E-5
for i=0,nst-1 do begin
x(i)=2.0*sin((lon(i)-lon(0))/360.0*2*pi/2.0)*a*cos(lat(i)/180.0*pi)
y(i)=2.0*sin((lat(i)-lat(0))/360.0*2*pi/2.0)*a
f(i)=2.0*omega*sin(lat(i)/180.0*pi)
endfor

z1=fltarr(nst)
dzdx=fltarr(nst)
dzdy=fltarr(nst)
for i=0,nst-1 do begin
z1(i)=1.0
line_fit_xyz,nst,x,y,z1,c1,c2,c3
dzdx(i)=c1
dzdy(i)=c2
z1(i)=0.0
endfor

x1=fltarr(nst+2)
y1=fltarr(nst+2)
divu=fltarr(nst)
divv=fltarr(nst)
for i=0,nst-1 do begin
x1(i+1)=x(i)
y1(i+1)=y(i)
endfor
x1(0)=x1(nst)
y1(0)=y1(nst)
x1(nst+1)=x1(1)
y1(nst+1)=y1(1)
area=0.0
for i=1,nst-2 do begin
ax=x(0)-x(i)
ay=y(0)-y(i)
bx=x(0)-x(i+1)
by=y(0)-y(i+1)
area=area+abs(ax*by-ay*bx)/2.0
endfor
divu(0)=0.0
divv(0)=0.0
for i=0,nst-1 do begin
divu(i)=(y1(i+2)-y1(i))/2.0/area
divv(i)=-(x1(i+2)-x1(i))/2.0/area
endfor
;stop
end
