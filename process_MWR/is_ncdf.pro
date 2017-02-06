;+
;
; (C) Cooperative Institute for Meteorological Satellite Studies, 1999
;
; NAME:
;       is_ncdf
;
; PURPOSE:
;       Fucntion to check if a file is a NetCDF file.
;
; CATEGORY:
;       NCDF
;
; LANGUAGE:
;       IDL v5
;
; CALLING SEQUENCE:
;       result = is_ncdf( file )
;
; INPUTS:
;       file:    Name of the input file to test.
;
; KEYWORD PARAMETERS:
;       None.
;
; OUTPUTS:
;       Function returns -1 if the file is NOT in NetCDF format or if
;                           any errors occur with input processing,
;                         1 if the file IS in NetCDF format
;
; CALLS:
;       None.
;
; COMMON BLOCKS:
;       None.
;
; SIDE EFFECTS:
;       None.
;
; RESTRICTIONS:
;       None known.
;
; PROCEDURE:
;       Function uses the CATCH procedure to handle the error that
;         may occur using NCDF_OPEN to open a non-NetCDF format file.
;
; EXAMPLE:
;       IDL> PRINT, is_ncdf( 'blah' )
;       % IS_NCDF: blah is not a NetCDF format file.
;             -1
;
;       IDL> PRINT, is_ncdf( '980711C2_RSE.nc' )
;              1
;
; MODIFICATION HISTORY:
;       Written by:     Paul van Delst, CIMSS/SSEC, 19-Apr-1999
;                       paul.vandelst@ssec.wisc.edu
;
;-

FUNCTION is_ncdf, file


;------------------------------------------------------------------------------
;                             -- RCS Id keyword --
;------------------------------------------------------------------------------

  rcs_Id = '$Id: is_ncdf.pro,v 1.2 1999/04/19 16:16:59 paulv Exp $'



;------------------------------------------------------------------------------
;                               -- Check input --
;------------------------------------------------------------------------------

  n_arguments = 1
  IF ( N_PARAMS() NE n_arguments) THEN BEGIN
    MESSAGE, 'Must supply an input filename', /INFO
    RETURN, -1
  ENDIF


; ---------------------------------
; Check if file argument is defined
; ---------------------------------

  IF ( N_ELEMENTS( file ) EQ 0 ) THEN BEGIN
    MESSAGE, 'File argument not defined!', /INFO
    RETURN, -1
  ENDIF

  IF ( STRLEN( file ) EQ 0 ) THEN BEGIN
    MESSAGE, 'File name string is zero length!', /INFO
    RETURN, -1
  ENDIF



;------------------------------------------------------------------------------
;                     -- Establish CATCH condition --
;------------------------------------------------------------------------------

  CATCH, error_status
  IF ( error_status NE 0 ) THEN BEGIN
    CATCH, /CANCEL
    RETURN, -1
  ENDIF

;------------------------------------------------------------------------------
;                        -- Open the input file --
;------------------------------------------------------------------------------

  ncdf_id = NCDF_OPEN( file )

  IF ( ncdf_id EQ -1 ) THEN BEGIN
    MESSAGE, 'Error opening file ' + file, /INFO
    RETURN, -1
  ENDIF



;------------------------------------------------------------------------------
;                         -- No errors occurred --
;------------------------------------------------------------------------------

; ------------------
; Turn off the CATCH
; ------------------

  CATCH, /CANCEL


; ---------------------
; Close the NetCDF file
; ---------------------

  NCDF_CLOSE, ncdf_id


; --------------
; Return success
; --------------

  RETURN, 1

END