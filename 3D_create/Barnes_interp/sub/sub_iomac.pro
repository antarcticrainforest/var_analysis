
	pro opt2mac,outputfile,nx,ny,x,y,d
;                                         d(x,y)
	  openw,1,outputfile
	  print,'output in 2mac: ', outputfile,nx,ny

          nz=0
          printf,1,format='(A1)',' '
          printf,1,format='(2I8)',ny,nx
          printf,1,format='(2I8)',nz,nz
          printf,1,format='(8f10.4)',reverse(y)
          printf,1,format='(8f10.4)',x
          printf,1,' '
          d1=fltarr(nx)
          for j=0,ny-1 do begin
            j1=ny-j-1
            d1(*)=d(*,j1)
            printf,1,format='(6E13.5)',d1
          endfor
          close,1
      end 


	pro ipt2mac,outputfile,nx,ny,x,y,d
;                                         d(x,y)
	  openr,1,outputfile
	  print,'input in 2mac: ', outputfile

          nz=0
          c=''
          readf,1,c
          readf,1,ny,nx
          readf,1,nz,nz
          x=fltarr(nx)
          z=fltarr(ny)
     
          d=fltarr(nx,ny)

          readf,1,format='(8f10.4)',z
          y=reverse(z)

          readf,1,format='(8f10.4)',x
          readf,1,c
          d1=fltarr(nx)
      
          for j=0,ny-1 do begin
            j1=ny-j-1
            
            readf,1,format='(6E13.5)',d1
            d(*,j1)=d1(*)
          endfor
          close,1
      end 


	pro opt1mac,outputfile,c,nv,nt,d
;                                         d(nv,nt)
	  openw,1,outputfile
	  print,'output in 1mac: ', outputfile,nv,nt

          printf,1,c
          d1=fltarr(nv)
          for j=0,nt-1 do begin
            d1(*)=d(*,j)
            printf,1,d1
          endfor
          close,1
    end
