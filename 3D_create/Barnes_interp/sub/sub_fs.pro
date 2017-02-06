	pro fs,n,y,f,a,b
	  f=fltarr(n/2+1)
	  a=fltarr(n/2+1)
	  b=fltarr(n/2+1)

	  pi=3.1415926
	  for k=0,n/2 do begin
	    u=2*pi*k/(n+0.0)
	    f(k)=k/(n+0.0)
	    a(k)=0
	    b(k)=0
	    for i=0,n-1 do begin
	      a(k)=a(k)+y(i)*cos(u*i)
	      b(k)=b(k)+y(i)*sin(u*i)
	    endfor
	    a(k)=a(k)/n*2.0
	    b(k)=b(k)/n*2.0
	    if(k eq 0 or k eq n/2) then begin
	      a(k)=a(k)*0.5
	    endif
	    if(k eq n/2) then begin
	      b(k)=0
	    endif
	  endfor

	end

	pro inv_fs,n,a,b,k1,k2,y
	  y=fltarr(n)

	  pi=3.1415926
	  for i=0,n-1 do begin
	    y(i)=0
	    for k=k1,k2 do begin
	      u=2*pi*k/(n+0.0)
	      y(i)=y(i)+a(k)*cos(u*i)+b(k)*sin(u*i)
	    endfor
	  endfor
	end
