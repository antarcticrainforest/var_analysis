
  ; A.B/|A||B| =cos(theta)
  ; A.B/|B| = A_projB
  ; perpendicular mode
  ;| (A-A_ProjB*B/|B|)/|B| |  =?=  |A|*sin(theta)/|B|     !no

 pro projection, a,b,  am,  bm,c0,  c1, c2,  c3,  c4
;                     mode/N  corr rms cos  parallel  perpendicular 

 c0 = c_correlate(a,b,0)
 
 c1 = sqrt(total( (a-b)^2 ) )/n_elements(a)

 am = sqrt(total(a^2) )
 bm = sqrt(total(b^2) )
 ab = total(a*b)

 c2 = ab/(am*bm)
 
 c = ab/am *a /am      ;projected vector
 c = b - c             ;difference vector
 c = c/am              ;normalized
 

 c3 = ab/(am*am)       ; projection ratio
 c4 = sqrt ( total(c^2) )

 am = am/n_elements(a)
 bm = bm/n_elements(b)

 end
 

