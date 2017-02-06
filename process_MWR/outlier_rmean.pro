FUNCTION OUTLIER_RMEAN, arr, width, limit

;this function searches for outliers in ARR using ...
; Elsaesser: , p280

; it first calculates a running mean of ARR using WIDTH
; as the averaging window
; value at the beginning and end are set to the nearest
; proper running mean value (+/- WIDTH/2)

; Then the outlier measure DEV is calculated and outliers
; are identified when DEV > LIMIT

;Input: ARR - 1d array of values
;       WIDTH - Window width for smoothing
;       LIMIT - Limit value for defintion of outliers

; Output: FLAG_ARR - 1d array with 0 - no outlier
;                                  1 - outlier

;check array length vs averaging window
n=N_ELEMENTS(arr)
IF(n LT width) THEN BEGIN
	PRINT,'Time series shorter than averaging window'
	STOP
ENDIF

;calculate running mean and fill array edges
rmean=SMOOTH(arr,width,/NAN)
FOR nx=0L,width/2-1 DO BEGIN
	rmean[nx]=rmean[width/2]
ENDFOR
FOR nx=n-width/2,n-1 DO BEGIN
	rmean[nx]=rmean[n-width/2-1]
ENDFOR

; calculate measure to define outliers (Elsaesser statistics book, p280)
dev=arr-rmean
sumdev=SQRT(TOTAL(dev^2))
dev=ABS(dev)/sumdev

; fill flag array
flag=FLTARR(n)
flag[0:n-1]=0
FOR nx=0L,n-1 DO BEGIN
	IF(dev[nx] GT limit) THEN flag[nx]=1
ENDFOR

RETURN, flag
END