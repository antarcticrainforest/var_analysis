pro wind,x,y,u,v,col,size=size

; the last wing resolved to true length


  if(abs(u)+abs(v) ge 200.)then begin
  xyouts,x,y,'x',color=1,alignment=0.5

    return
  endif

  xyouts,x,y,'o',alignment=0.5,color=5

  speed = (u*u+v*v)^0.5

  
  if(not keyword_set(size) ) then size = 2
  length = size *10     ;pixels

 win_length = length/3
 win_inteval= length/6

 xy_pixels = convert_coord([x,y],/data,/to_Device)
 
if(speed lt 0.01)then return
      
  cos_angle1=u/speed
  sin_angle1=v/speed
  
  dx_pixel = xy_pixels[0] - length * cos_angle1
  dy_pixel = xy_pixels[1] - length * sin_angle1

  dxdy_data = convert_coord([dx_pixel,dy_pixel],/device,/to_data)

  x1 = dxdy_data[0]
  y1 = dxdy_data[1]
  oplot,[x1,x],[y1,y],color=col

 if(speed lt 20) then begin
; -----------------------------
  k=fix(speed) /  4  

   for i=1,k do begin
     x1_pixel = xy_pixels[0]-cos_angle1*(length-(i-1)*win_inteval)
     y1_pixel = xy_pixels[1]-sin_angle1*(length-(i-1)*win_inteval)
     
     x2_pixel = x1_pixel - sin_angle1*win_length
     y2_pixel = y1_pixel + cos_angle1*win_length

     x1y1 = convert_coord([x1_pixel,y1_pixel],/device,/to_data)
     x2y2 = convert_coord([x2_pixel,y2_pixel],/device,/to_data)

     oplot,[x1y1[0],x2y2[0]],[x1y1[1],x2y2[1]],color=col

   endfor 

     i=k+1
     x1_pixel = xy_pixels[0]-cos_angle1*(length-(i-1)*win_inteval)
     y1_pixel = xy_pixels[1]-sin_angle1*(length-(i-1)*win_inteval)
     
     x2_pixel = x1_pixel - sin_angle1*win_length * (speed - k*4.)/4.
     y2_pixel = y1_pixel + cos_angle1*win_length * (speed - k*4.)/4.

     x1y1 = convert_coord([x1_pixel,y1_pixel],/device,/to_data)
     x2y2 = convert_coord([x2_pixel,y2_pixel],/device,/to_data)

     oplot,[x1y1[0],x2y2[0]],[x1y1[1],x2y2[1]],color=col
   

 endif else begin      ; speed larger than 20 m/s
; ------------------------------------------------------

  k2=fix(speed) /  20
  k=  (fix(speed) - k2*20 )/4  
  


   for i=1,k2 do begin
     x1_pixel = xy_pixels[0]-cos_angle1*(length-(i-1)*win_inteval)
     y1_pixel = xy_pixels[1]-sin_angle1*(length-(i-1)*win_inteval)
     
     x2_pixel = x1_pixel - sin_angle1*win_length
     y2_pixel = y1_pixel + cos_angle1*win_length



     x3_pixel = xy_pixels[0]-cos_angle1*(length-i*win_inteval)
     y3_pixel = xy_pixels[1]-sin_angle1*(length-i*win_inteval)
     
     x4_pixel = x3_pixel - sin_angle1*win_length
     y4_pixel = y3_pixel + cos_angle1*win_length


     x2_pixel = (x2_pixel + x4_pixel)/2.
     y2_pixel = (y2_pixel + y4_pixel)/2.
 
     x1y1 = convert_coord([x1_pixel,y1_pixel],/device,/to_data)
     x2y2 = convert_coord([x2_pixel,y2_pixel],/device,/to_data)
     x3y3 = convert_coord([x3_pixel,y3_pixel],/device,/to_data)

     oplot,[x1y1[0],x2y2[0]],[x1y1[1],x2y2[1]],color=col
     oplot,[x3y3[0],x2y2[0]],[x3y3[1],x2y2[1]],color=col
   endfor 

   for i=1,k do begin
     x1_pixel = xy_pixels[0]-cos_angle1*(length-(k2+i-0.5)*win_inteval)
     y1_pixel = xy_pixels[1]-sin_angle1*(length-(k2+i-0.5)*win_inteval)
     
     x2_pixel = x1_pixel - sin_angle1*win_length
     y2_pixel = y1_pixel + cos_angle1*win_length

     x1y1 = convert_coord([x1_pixel,y1_pixel],/device,/to_data)
     x2y2 = convert_coord([x2_pixel,y2_pixel],/device,/to_data)

     oplot,[x1y1[0],x2y2[0]],[x1y1[1],x2y2[1]],color=col
   endfor 

        
     i=k+1
     x1_pixel = xy_pixels[0]-cos_angle1*(length-(k2+i-0.5)*win_inteval)
     y1_pixel = xy_pixels[1]-sin_angle1*(length-(k2+i-0.5)*win_inteval)

     x2_pixel = x1_pixel - sin_angle1*win_length*(speed-k2*20.-k*4.)/4.
     y2_pixel = y1_pixel + cos_angle1*win_length*(speed-k2*20.-k*4.)/4.

     x1y1 = convert_coord([x1_pixel,y1_pixel],/device,/to_data)
     x2y2 = convert_coord([x2_pixel,y2_pixel],/device,/to_data)

     oplot,[x1y1[0],x2y2[0]],[x1y1[1],x2y2[1]],color=col
 

 endelse
;---------------------------------------

 return

 end
  


pro wind2,x,y,u,v,col,size=size

; 2 m/s resolution

 xyouts,x,y,'o'

; plot,t,d(5,5,1,*),yrange=[-30,30],psym=2

  speed = (u*u+v*v)^0.5

  
  if(not keyword_set(size) ) then size = 2
  length = size *10     ;pixels

 win_length = length/3
 win_inteval= length/6

 xy_pixels = convert_coord([x,y],/data,/to_Device)
 
if(speed lt 2.0)then return
      
  cos_angle1=u/speed
  sin_angle1=v/speed
  
  dx_pixel = xy_pixels[0] - length * cos_angle1
  dy_pixel = xy_pixels[1] - length * sin_angle1

  dxdy_data = convert_coord([dx_pixel,dy_pixel],/device,/to_data)

  x1 = dxdy_data[0]
  y1 = dxdy_data[1]
  oplot,[x1,x],[y1,y],color=col

 if(speed lt 20) then begin
; -----------------------------
  k=fix(speed) /  2  
  k3 = k/2

   for i=1,k3 do begin
     x1_pixel = xy_pixels[0]-cos_angle1*(length-(i-1)*win_inteval)
     y1_pixel = xy_pixels[1]-sin_angle1*(length-(i-1)*win_inteval)
     
     x2_pixel = x1_pixel - sin_angle1*win_length
     y2_pixel = y1_pixel + cos_angle1*win_length

     x1y1 = convert_coord([x1_pixel,y1_pixel],/device,/to_data)
     x2y2 = convert_coord([x2_pixel,y2_pixel],/device,/to_data)

     oplot,[x1y1[0],x2y2[0]],[x1y1[1],x2y2[1]],color=col

   endfor 
   if((k mod 2) eq 1) then begin       ;  odd
     i=k3+1
     x1_pixel = xy_pixels[0]-cos_angle1*(length-(i-1)*win_inteval)
     y1_pixel = xy_pixels[1]-sin_angle1*(length-(i-1)*win_inteval)
     
     x2_pixel = x1_pixel - sin_angle1*win_length/2.
     y2_pixel = y1_pixel + cos_angle1*win_length/2.

     x1y1 = convert_coord([x1_pixel,y1_pixel],/device,/to_data)
     x2y2 = convert_coord([x2_pixel,y2_pixel],/device,/to_data)

     oplot,[x1y1[0],x2y2[0]],[x1y1[1],x2y2[1]],color=col
   endif    

 endif else begin      ; speed larger than 20 m/s
; ------------------------------------------------------

  k2=fix(speed) /  20
  k=  (fix(speed) - k2*20 )/2  
  k3 = k/2


   for i=1,k2 do begin
     x1_pixel = xy_pixels[0]-cos_angle1*(length-(i-1)*win_inteval)
     y1_pixel = xy_pixels[1]-sin_angle1*(length-(i-1)*win_inteval)
     
     x2_pixel = x1_pixel - sin_angle1*win_length
     y2_pixel = y1_pixel + cos_angle1*win_length



     x3_pixel = xy_pixels[0]-cos_angle1*(length-i*win_inteval)
     y3_pixel = xy_pixels[1]-sin_angle1*(length-i*win_inteval)
     
     x4_pixel = x3_pixel - sin_angle1*win_length
     y4_pixel = y3_pixel + cos_angle1*win_length


     x2_pixel = (x2_pixel + x4_pixel)/2.
     y2_pixel = (y2_pixel + y4_pixel)/2.
 
     x1y1 = convert_coord([x1_pixel,y1_pixel],/device,/to_data)
     x2y2 = convert_coord([x2_pixel,y2_pixel],/device,/to_data)
     x3y3 = convert_coord([x3_pixel,y3_pixel],/device,/to_data)

     oplot,[x1y1[0],x2y2[0]],[x1y1[1],x2y2[1]],color=col
     oplot,[x3y3[0],x2y2[0]],[x3y3[1],x2y2[1]],color=col
   endfor 

   for i=1,k3 do begin
     x1_pixel = xy_pixels[0]-cos_angle1*(length-(k2+i-0.5)*win_inteval)
     y1_pixel = xy_pixels[1]-sin_angle1*(length-(k2+i-0.5)*win_inteval)
     
     x2_pixel = x1_pixel - sin_angle1*win_length
     y2_pixel = y1_pixel + cos_angle1*win_length

     x1y1 = convert_coord([x1_pixel,y1_pixel],/device,/to_data)
     x2y2 = convert_coord([x2_pixel,y2_pixel],/device,/to_data)

     oplot,[x1y1[0],x2y2[0]],[x1y1[1],x2y2[1]],color=col
   endfor 

   if((k mod 2) eq 1) then begin       ;  odd
     i=k3+1
     x1_pixel = xy_pixels[0]-cos_angle1*(length-(k2+i-0.5)*win_inteval)
     y1_pixel = xy_pixels[1]-sin_angle1*(length-(k2+i-0.5)*win_inteval)
     
     x2_pixel = x1_pixel - sin_angle1*win_length/2.
     y2_pixel = y1_pixel + cos_angle1*win_length/2.

     x1y1 = convert_coord([x1_pixel,y1_pixel],/device,/to_data)
     x2y2 = convert_coord([x2_pixel,y2_pixel],/device,/to_data)

     oplot,[x1y1[0],x2y2[0]],[x1y1[1],x2y2[1]],color=col
   endif    

 endelse
;---------------------------------------

 return

 end
  


pro arrow,x,y,dx,dy,col

pi=3.1416
x1=x-dx
y1=y-dy
length=(dx*dx+dy*dy)^0.5
length_head=length*0.2
cos_angle1=dx/length
sin_angle1=dy/length
cos_angle2=cos_angle1*0.866025+sin_angle1*0.5
sin_angle2=sin_angle1*0.866025-cos_angle1*0.5
cos_angle3=cos_angle1*0.866025-sin_angle1*0.5
sin_angle3=sin_angle1*0.866025+cos_angle1*0.5
x2=x-length_head*cos_angle2
y2=y-length_head*sin_angle2
x3=x-length_head*cos_angle3
y3=y-length_head*sin_angle3

oplot,[x1,x],[y1,y],color=col
oplot,[x2,x],[y2,y],color=col
oplot,[x3,x],[y3,y],color=col

end


pro running_mean,n,y,na,y2

y2=fltarr(n)
for l=0,na-1 do begin
y2(l)=total(y(0:2*l))/(2*l+1.0)
endfor
for l=na,n-na-1 do begin
y2(l)=total(y(l-na:l+na))/(2*na+1.0)
endfor
for l=n-na,n-1 do begin
y2(l)=total(y(2*l-n+1:n-1))/(2*(n-1-l)+1.0)
endfor

end


pro plot_line,x,y,len,sty,str,siz

oplot,[x,x+len],[y,y],line=sty
xyouts,x+len*1.1,y,str,size=siz

end


pro star,x,y,w,color

d=0.5*w
x1=[x,x]
y1=[y-d,y+d]
x2=[x-d,x+d]
y2=[y,y]
x3=[x-d,x+d]
y3=[y-d,y+d]
x4=[x-d,x+d]
y4=[y+d,y-d]
oplot,x1,y1,color=color
oplot,x2,y2,color=color
oplot,x3,y3,color=color
oplot,x4,y4,color=color

end


pro cross,x,y,w,color

d=0.5*w
x1=[x,x]
y1=[y-d,y+d]
x2=[x-d,x+d]
y2=[y,y]
oplot,x1,y1,color=color
oplot,x2,y2,color=color

end


pro circle,x,y,r,n,color

d=(r+0.0)/n
for i=-n,n do begin
for j=-n,n do begin
dx=i*d
dy=j*d
r1=(dx*dx+dy*dy)^0.5
if(r1 le r) then begin
xyouts,x+dx,y+dy,'.',size=d,col=color
endif
endfor
endfor

end


pro circle1,x,y,r,n,color,a,b

d=(r+0.0)/n
for i=-n,n do begin
for j=-n,n do begin
dx=i*d*a
dy=j*d*b
r1=(dx*dx+dy*dy)^0.5
if(r1 le r) then begin
xyouts,x+dx/a,y+dy/b,'.',size=d,col=color
endif
endfor
endfor

end


pro circle2,x,y,r,dr,n,color,a,b

d=(r+0.0)/n
for i=-n,n do begin
for j=-n,n do begin
dx=i*d*a
dy=j*d*b
r1=(dx*dx+dy*dy)^0.5
if(r1 le (r+dr) and r1 ge (r-dr)) then begin
xyouts,x+dx/a,y+dy/b,'.',size=d,col=color
endif
endfor
endfor

end


pro arc4,x,y,r,dr,n,color,a,b

d=(r+0.0)/n
for i=0,n do begin
for j=-n,0 do begin
dx=i*d*a
dy=j*d*b
r1=(dx*dx+dy*dy)^0.5
if(r1 le (r+dr) and r1 ge (r-dr)) then begin
xyouts,x+dx/a,y+dy/b,'.',size=d,col=color
endif
endfor
endfor

end


pro arc3,x,y,r,dr,n,color,a,b

d=(r+0.0)/n
for i=-n,0 do begin
for j=-n,0 do begin
dx=i*d*a
dy=j*d*b
r1=(dx*dx+dy*dy)^0.5
if(r1 le (r+dr) and r1 ge (r-dr)) then begin
xyouts,x+dx/a,y+dy/b,'.',size=d,col=color
endif
endfor
endfor

end


pro box,x,y,w,h,color

x1=x-0.5*w
x2=x+0.5*w
y1=y-0.5*h
y2=y+0.5*h
x3=[x1,x2,x2,x1,x1]
y3=[y1,y1,y2,y2,y1]
oplot,x3,y3
polyfill,[x1,x2,x2,x1],[y1,y1,y2,y2],col=color
end

pro box2,x,y,w,h,col

x1=x-0.5*w
x2=x+0.5*w
y1=y-0.5*h
y2=y+0.5*h
x3=[x1,x2,x2,x1,x1]
y3=[y1,y1,y2,y2,y1]
oplot,x3,y3,color=col
end

pro slant_box,x,y,w,w1,h,col

x1=x-0.5*w
x2=x+0.5*w
y1=y-0.5*h
y2=y+0.5*h
x3=[x1,x2,x2+w1,x1+w1,x1]
y3=[y1,y1,y2,y2,y1]
oplot,x3,y3,color=col
polyfill,[x1,x2,x2+w1,x1+w1],[y1,y1,y2,y2],col=col
end

pro triangle,x,y,w,h,color

x1=x-0.5*w
x2=x+0.5*w
x3=x
y1=y-0.5*h
y2=y-0.5*h
y3=y+0.5*h

x=[x1,x2,x3,x1]
y=[y1,y2,y3,y1]
oplot,x,y
polyfill,[x1,x2,x3],[y1,y2,y3],col=color
end
