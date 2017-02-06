function is_leap,year
  if (year mod 400 eq 0) then begin
    leap = 1
  endif else if (year mod 100 eq 0) then begin
    leap = 0
  endif else if (year mod 4 eq 0) then begin
    leap = 1
  endif else begin
    leap = 0
  endelse
  return, leap
end
