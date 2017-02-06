;+
;
; (C) Cooperative Institute for Meteorological Satellite Studies, 1999
;
; NAME:
;       read_ncdf
;
; PURPOSE:
;       Function to read variable and attribute data from NetCDF
;       format files.
;
; CATEGORY:
;       NCDF
;
; LANGUAGE:
;       IDL v5
;
; CALLING SEQUENCE:
;       result = read_ncdf( ncdf_file, $                                     ; Input
;                           data, $                                          ; Output
;                           variable_list         = variable_list, $         ; Input keyword
;                           count                 = count, $                 ; Input keyword
;                           offset                = offset, $                ; Input keyword
;                           stride                = stride, $                ; Input keyword
;                           variable_attributes   = variable_attributes, $   ; Input keyword
;                           global_attributes     = global_attributes, $     ; Input keyword
;                           no_var_byte_to_string = no_var_byte_to_string, $ ; Input keyword
;                           no_att_byte_to_string = no_att_byte_to_string, $ ; Input keyword
;                           quiet                 = quiet )                  ; Input keyword
;
; INPUTS:
;       ncdf_file:     The name of the NetCDF file to read
;
; INPUT KEYWORD PARAMETERS:
;       variable_list:          A string array of variable name to read from
;                               the NetCDF file. If not specified, ALL the
;                               variables are read.
;       count:                  Set this keyword to a vector containing the
;                               number of points in each dimension that are
;                               required for a variable read. It is a 1-based
;                               vector and defaults to match the size of all
;                               dimensions so that all data is read.
;       offset:                 Set this keyword to a vector containing the
;                               starting index position for each dimension of
;                               the variable required. It is a 0-based
;                               vector and defaults to zero for every dimension
;                               so that all data is read.
;       stride:                 Set this keyword to a vector containing the
;                               strides, or sampling intervals, between accessed
;                               values of the required variable. It is a 1-based
;                               vector and defaults to one for every dimension
;                               so that all data is read.
;       variable_attributes:    Set this keyword to return variable
;                               attribute data also.
;       global_attributes:      Set this keyword to return global
;                               attribute data. NO DATA IS READ IF THIS
;                               KEYORD IS SET.
;       no_var_byte_to_string:  Set this keyword to prevent the
;                               conversion of BYTE variable data
;                               to STRING type. (IDL 5.2 and earlier only)
;       no_att_byte_to_string:  Set this keyword to prevent the
;                               conversion of BYTE attribute data
;                               to STRING type. (IDL 5.2 and earlier only)
;       quiet:                  Set this keyword to suppress informational
;                               output.
;
; OUTPUTS:
;       data:          The data structure containing the file data
;                      requested.
;
;                      OUTPUT DATA STRUCTURE FORM
;                      --------------------------
;                      o If only variable data is read in, the output structure
;                        has the form:
;
;                          data.var1
;                              .var2
;                              .var3
;                            .....
;                              .varN
;
;                      o If variable attributes are also requested, the output
;                        structure has the form:
;
;                          data.var1.data
;                                   .att1
;                                   .att2
;                                 .....
;                                   .attN
;                              .var2.data
;                                   .att1
;                                   .att2
;                                 .....
;                                   .attN
;                            .....
;                              .varN.data
;                                   .att1
;                                   .att2
;                                 .....
;                                   .attN
;
;                      o If global attributes are requested, the output
;                        structure has the form:
;
;                          data.gatt1
;                              .gatt2
;                              .gatt3
;                            .....
;                              .gattN
;
;
;       Function returns a flag:
;         result = -1 Error occurred during NetCDF read,
;                =  1 Everything worked fine

; OUTPUT KEYWORD PARAMETERS:
;       None.
;
; CALLS:
;       is_ncdf:   Function to determine if a file is NetCDF format.
;
; COMMON BLOCKS:
;       None.
;
; SIDE EFFECTS:
;       None.
;
; RESTRICTIONS:
;       None.
;
; PROCEDURE:
;       As each variable/attribute is read in, it is appended to the output data
;       structure.
;
; EXAMPLE:
;       Reading a list of variables AND THEIR ATTRIBUTES from a NetCDF file:
;
;         IDL> PRINT, read_ncdf( file, $
;         IDL>                   data, $
;         IDL>                   variable_list = ['Pressure_Levels', $
;         IDL>                                    'Latitude','Longitude', $
;         IDL>                                    'Local_Zenith_Angle', $
;         IDL>                                    'Temperature_Retrieval', $
;         IDL>                                    'WaterVapor_Retrieval' ], $
;         IDL>                   /variable_attributes )
;                   Number of dimensions        :       7
;                   Number of variables         :      36
;                   Number of global attributes :       8
;                   ID of unlimited dimension   :       6
;                1
;
;       gives the following data structure:
;
;         IDL> HELP, data, /STRUCT
;         ** Structure <104fb008>, 6 tags, length=1647936, refs=1:
;            PRESSURE_LEVELS STRUCT    -> <Anonymous> Array[1]
;            LATITUDE        STRUCT    -> <Anonymous> Array[1]
;            LONGITUDE       STRUCT    -> <Anonymous> Array[1]
;            LOCAL_ZENITH_ANGLE
;                            STRUCT    = -> <Anonymous> Array[1]
;            TEMPERATURE_RETRIEVAL
;                            STRUCT    = -> <Anonymous> Array[1]
;            WATERVAPOR_RETRIEVAL
;                            STRUCT    = -> <Anonymous> Array[1]
;
;       A single variable structure, with attributes:
;
;         IDL> HELP, data.temperature_retrieval, /STRUCT
;         ** Structure <10340c08>, 8 tags, length=795368, refs=2:
;            DATA            FLOAT     Array[42, 18, 263]
;            LONG_NAME       STRING    'Temperature Retrieval for the IAPP'
;            UNITS           STRING    'degrees kelvin'
;            SCALE_FACTOR    DOUBLE           1.0000000
;            ADD_OFFSET      DOUBLE           0.0000000
;            PARAMETER_TYPE  STRING    'IAPP Output'
;            VALID_RANGE     FLOAT     Array[2]
;            _FILLVALUE      FLOAT          -999.000
;
;       Reading just the variable data, i.e. NO /VARIABLE_ATTRIBUTES keyword
;       produces the following output:
;
;         IDL> HELP, data, /STRUCT
;         ** Structure <10339408>, 6 tags, length=1647600, refs=1:
;            PRESSURE_LEVELS FLOAT     Array[42]
;            LATITUDE        FLOAT     Array[18, 263]
;            LONGITUDE       FLOAT     Array[18, 263]
;            LOCAL_ZENITH_ANGLE
;                            FLOAT     = Array[18, 263]
;            TEMPERATURE_RETRIEVAL
;                            FLOAT     = Array[42, 18, 263]
;            WATERVAPOR_RETRIEVAL
;                            FLOAT     = Array[42, 18, 263]
;
; CREATION HISTORY:
;       Written by:     Paul van Delst, CIMSS/SSEC, 23-Sep-1999
;                       paul.vandelst@ssec.wisc.edu
;
;-

;===============================================================================
;
; Function to check the validity of the COUNT, OFFSET, and STRIDE vector
; keywords.
;
; Adapted from Liam Gumley's NC_READ.PRO
;
;===============================================================================

FUNCTION check_vectors, ncdf_id, $
                        variable_info, $
                        count, $
                        offset, $
                        stride, $
                        count_vector, $
                        offset_vector, $
                        stride_vector



;------------------------------------------------------------------------------
;                         -- Get the variable dimensions --
;------------------------------------------------------------------------------

; -----------------------------------
; Create the variable dimension array
; -----------------------------------

  variable_dimensions = LONARR( variable_info.NDIMS )


; ------------------------
; Loop over the dimensions
; ------------------------

  FOR i = 0, variable_info.NDIMS - 1 DO BEGIN

;   -- Get the dimension name and size
    NCDF_DIMINQ, ncdf_id, $                 ; Input
                 variable_info.DIM[ i ], $  ; Input
                 dimension_name, $          ; Output
                 dimension_size             ; Output

;   -- Save the dimension size
    variable_dimensions[ i ] = dimension_size

  ENDFOR



;------------------------------------------------------------------------------
;    -- Check for COUNT, OFFSET, and STRIDE vectors of the wrong length --
;------------------------------------------------------------------------------

; ------------
; Count vector
; ------------

  IF ( N_ELEMENTS( count ) NE 0 AND $
       N_ELEMENTS( count ) NE variable_info.NDIMS ) THEN BEGIN
    MESSAGE, 'COUNT vector must have ' + $
             STRTRIM( variable_info.NDIMS, 2 ) + $
             ' dimensions for ' + $
             STRUPCASE( variable_info.NAME ) + $
             ' variable', /INFO
    RETURN, -1
  ENDIF


; -------------
; Offset vector
; -------------

  IF ( N_ELEMENTS( offset ) NE 0 AND $
       N_ELEMENTS( offset ) NE variable_info.NDIMS ) THEN BEGIN
    MESSAGE, 'OFFSET vector must have ' + $
             STRTRIM( variable_info.NDIMS, 2 ) + $
             ' dimensions for ' + $
             STRUPCASE( variable_info.NAME ) + $
             ' variable', /INFO
    RETURN, -1
  ENDIF


; -------------
; Stride vector
; -------------

  IF ( N_ELEMENTS( stride ) NE 0 AND $
       N_ELEMENTS( stride ) NE variable_info.NDIMS ) THEN BEGIN
    MESSAGE, 'STRIDE vector must have ' + $
             STRTRIM( variable_info.NDIMS, 2 ) + $
             ' dimensions for ' + $
             STRUPCASE( variable_info.NAME ) + $
             ' variable', /INFO
    RETURN, -1
  ENDIF



;------------------------------------------------------------------------------
;  -- Check for definition and range of COUNT, OFFSET, and STRIDE vectors --
;------------------------------------------------------------------------------

; -------------
; Offset vector
; -------------

; -- Is it defined?
  IF ( N_ELEMENTS( offset ) EQ 0 ) THEN $
    offset_vector = REPLICATE( 0L, variable_info.NDIMS ) $
  ELSE $
    offset_vector = LONG( offset )

; -- Is it valid?
  offset_vector = ( offset_vector < ( variable_dimensions - 1L ) ) > $
                  REPLICATE( 0L, variable_info.NDIMS )


; -------------
; Stride vector
; -------------

; -- Is it defined?
  IF ( N_ELEMENTS( stride ) EQ 0 ) THEN $
    stride_vector = REPLICATE( 1L, variable_info.NDIMS ) $
  ELSE $
    stride_vector = LONG(stride)

; -- Is it valid?
  stride_vector = ( stride_vector < ( variable_dimensions - offset_vector ) ) > $
                  REPLICATE( 1L, variable_info.NDIMS )


; ------------
; Count vector
; ------------

; -- Is it defined?
  IF ( N_ELEMENTS( count ) EQ 0 ) THEN $
    count_vector = ( variable_dimensions - offset_vector ) / stride_vector $
  ELSE $
    count_vector = LONG( count )

; -- Is it valid?
  count_vector = ( count_vector < ( ( variable_dimensions - offset_vector ) / stride_vector ) ) > $
                 REPLICATE( 1L, variable_info.NDIMS )



;------------------------------------------------------------------------------
;                                  -- Done --
;------------------------------------------------------------------------------

  RETURN, 1

END




;===============================================================================
;
; Main function
;
;===============================================================================

FUNCTION read_ncdf, ncdf_file, $         ; Input
                    data, $              ; Output

;                   -- Which variables to read keyword
                    variable_list = variable_list, $    ; Input keyword

;                   -- How to read the variables keywords
                    count  = count, $    ; Input keyword
                    offset = offset, $   ; Input keyword
                    stride = stride, $   ; Input keyword

;                   -- Attribute keywords
                    variable_attributes = variable_attributes, $    ; Input keyword
                    global_attributes   = global_attributes, $      ; Input keyword

;                   -- Conversion keywords
                    no_var_byte_to_string = no_var_byte_to_string, $   ; Input keyword
                    no_att_byte_to_string = no_att_byte_to_string, $   ; Input keyword

;                   -- Shhhhh.
                    quiet = quiet        ; Input keyword



;------------------------------------------------------------------------------
;                             -- RCS Id keyword --
;------------------------------------------------------------------------------

  rcs_Id = '$Id: read_ncdf.pro,v 2.1 1999/10/08 16:45:02 paulv Exp $'



;------------------------------------------------------------------------------
;                   -- Set floating point precision --
;------------------------------------------------------------------------------

  tolerance = ( MACHAR( /DOUBLE ) ).EPS



;------------------------------------------------------------------------------
;                            -- Check input --
;------------------------------------------------------------------------------

  n_arguments = 2
  IF ( N_PARAMS() LT n_arguments ) THEN BEGIN
    MESSAGE, 'Invlaid number of arguments', /INFO
    RETURN, -1

  ENDIF



; -----------------------------------------
; Check that required arguments are defined
; -----------------------------------------

  IF ( N_ELEMENTS( ncdf_file ) EQ 0 ) THEN BEGIN
    MESSAGE, 'Input NCDF_FILE argument not defined!', /INFO
    RETURN, -1
  ENDIF


; ------------------------------------
; Check that file argument is a string
; ------------------------------------

  IF ( SIZE( ncdf_file, /TNAME ) NE 'STRING' ) THEN BEGIN
    MESSAGE, 'Input NCDF_FILE argument must be a string', /INFO
    RETURN, -1
  ENDIF


; -------------------------------------------------
; Check variable_list keyword. If the variable_list
; keyword is NOT set, the default action is to read
; ALL the data in the NetCDF file
; -------------------------------------------------

  IF ( KEYWORD_SET( variable_list ) ) THEN $
    all_variables = 0 $
  ELSE $
    all_variables = 1


; -------------------------------------------------
; Check global_attributes keword. If the keyword is
; set, this over-rides any variable data requests.
; -------------------------------------------------

  IF ( KEYWORD_SET( global_attributes ) ) THEN $
    read_data = 0 $
  ELSE $
    read_data = 1



;------------------------------------------------------------------------------
;                   -- Make sure that file is in NetCDF format --
;------------------------------------------------------------------------------

  result = is_ncdf( ncdf_file )

  IF ( result EQ -1 ) THEN BEGIN
    MESSAGE, ncdf_file + ' is not a NetCDF format file', /INFO
    RETURN, -1
  ENDIF



;------------------------------------------------------------------------------
;                        -- Open the netCDF data file --
;------------------------------------------------------------------------------

  ncdf_id = NCDF_OPEN( ncdf_file, /NOWRITE )

  IF ( ncdf_id EQ -1 ) THEN BEGIN
    MESSAGE, 'Error opening file ' + ncdf_file, /INFO
    RETURN, -1
  ENDIF



;------------------------------------------------------------------------------
;                         -- Set up error handler --
;------------------------------------------------------------------------------

  CATCH, error_status

  IF ( error_status NE 0 ) THEN BEGIN
    NCDF_CLOSE, ncdf_id
    CATCH, /CANCEL
    MESSAGE, !ERR_STRING, /INFO
    RETURN, -1
  ENDIF



;------------------------------------------------------------------------------
;                   -- Print out some dimension information --
;------------------------------------------------------------------------------

  ncdf_file_info = NCDF_INQUIRE( ncdf_id )

  IF ( NOT KEYWORD_SET( quiet ) ) THEN BEGIN

    PRINT, FORMAT = '( 10x,"Number of dimensions        : ",i7 )', $
                    ncdf_file_info.NDIMS
    PRINT, FORMAT = '( 10x,"Number of variables         : ",i7 )', $
                    ncdf_file_info.NVARS
    PRINT, FORMAT = '( 10x,"Number of global attributes : ",i7 )', $
                    ncdf_file_info.NGATTS
    PRINT, FORMAT = '( 10x,"ID of unlimited dimension   : ",i7 )', $
                    ncdf_file_info.RECDIM

  ENDIF



;------------------------------------------------------------------------------
;        -- Define the IDL version number before which STRINGS=BYTES --
;------------------------------------------------------------------------------

  fixed_IDL_version = 5.3



;------------------------------------------------------------------------------
;                    -- Read the global attributes or data? --
;------------------------------------------------------------------------------

  CASE read_data OF


;   --------------------------
;   Read the global attributes
;   --------------------------

    0: BEGIN


;     -----------------------------------------
;     Determine the number of global attributes
;     -----------------------------------------

      n_global_attributes = ncdf_file_info.NGATTS


;     ----------------------------------------
;     Are there any global attributes to read?
;     ----------------------------------------

      IF ( n_global_attributes GT 0 ) THEN BEGIN


;       ---------------------------
;       loop over global attributes
;       ---------------------------

        FOR i = 0, n_global_attributes - 1 DO BEGIN


;         -------------------------
;         Get global attribute name
;         -------------------------

          attribute_name = NCDF_ATTNAME( ncdf_id, $  ; Input
                                         i, $        ; Input
                                         /GLOBAL )   ; Input keyword


;         --------------------------
;         Get global attribute value
;         --------------------------

          NCDF_ATTGET, ncdf_id, $         ; Input
                       attribute_name, $  ; Input
                       attribute, $       ; Output
                       /GLOBAL            ; Input keyword


;         -------------------------
;         Get global attribute info
;         -------------------------

          attribute_info = NCDF_ATTINQ( ncdf_id, $         ; Input
                                        attribute_name, $  ; Input
                                        /GLOBAL )          ; Input keyword


;         -----------------------------------------------------------
;         If necessary and required, convert BYTE attribute to STRING
;         -----------------------------------------------------------

          IF ( FLOAT( !VERSION.RELEASE ) LT fixed_IDL_version  AND $    ; IDL version that differentiates BYTE and STRING NetCDF variables
               STRUPCASE( attribute_info.datatype ) EQ 'BYTE' AND $     ; Is the data a BYTE type?
               ( NOT KEYWORD_SET( no_att_byte_to_string ) ) ) THEN $    ; Is the conversion keyword set?
            attribute = STRING( attribute )


;         -------------------------------------------
;         Create/append to GLOBAL attribute structure
;         -------------------------------------------

          IF ( i GT 0 ) THEN BEGIN

;           -- Append to existing structure
            data = CREATE_STRUCT( data, $                ; Input
                                  attribute_name, $      ; Input
                                  attribute )            ; Input

          ENDIF ELSE BEGIN

;           -- Create new structure
            data = CREATE_STRUCT( attribute_name, $      ; Input
                                  attribute )            ; Input

          ENDELSE

        ENDFOR      ; Loop over global attributes


;     ----------------------------
;     No global attributes to read
;     ----------------------------

      ENDIF ELSE BEGIN

        MESSAGE, 'No global attributes to read!', /INFO

      ENDELSE     ; n_global_attributes > 0 IF statement

    END         ; Global attribute read CASE select.



;   -------------
;   Read the data
;   -------------

    1: BEGIN


;     ---------------------------------------
;     Set the number of variables to read and
;     initialise the valid variable counter
;     ---------------------------------------

      IF ( all_variables EQ 0 ) THEN $
        n_variables = N_ELEMENTS( variable_list ) $
      ELSE $
        n_variables = ncdf_file_info.NVARS


;     --------------------------------
;     Are there any variables to read?
;     --------------------------------

      IF ( n_variables GT 0 ) THEN BEGIN


;       -------------------
;       Loop over variables
;       -------------------

        FOR i = 0, n_variables - 1 DO BEGIN


;         -------------------
;         Get the variable ID
;         -------------------

          IF ( all_variables EQ 0 ) THEN BEGIN


;           ------------------------------
;           Only getting requested data so
;           set the current variable name
;           ------------------------------

            variable_name = variable_list[ i ]


;           ---------------------------
;           Get the current variable ID
;           ---------------------------

            variable_id = NCDF_VARID( ncdf_id, $       ; Input
                                      variable_name )  ; Input


;           ---------------------------------------------------
;           Is the current variable present in the NetCDF file?
;           ---------------------------------------------------

            IF ( variable_id EQ -1 ) THEN BEGIN
              NCDF_CLOSE, ncdf_id
              CATCH, /CANCEL
              MESSAGE, 'Variable ' + variable_name + ' not present in ' + ncdf_file, /INFO
              RETURN, -1
            ENDIF


          ENDIF ELSE BEGIN


;           -------------------------------------------------
;           Getting all data. Use loop counter as variable ID
;           -------------------------------------------------

            variable_id = i

          ENDELSE


;         ---------------------
;         Get the variable info
;         ---------------------

          variable_info = NCDF_VARINQ( ncdf_id, $     ; Input
                                       variable_id )  ; Input


;         -----------------------------------
;         Make sure we have the variable name
;         -----------------------------------

          variable_name = variable_info.NAME


;         ------------------------------------------
;         Does the current variable have dimensions?
;         ------------------------------------------

          IF ( variable_info.NDIMS EQ 0 ) THEN BEGIN


;           ---------------------------------
;           No. It is scalar. Simply read it.
;           ---------------------------------

            NCDF_VARGET, ncdf_id, $          ; Input
                         variable_id, $      ; Input
                         variable_data       ; Output

          ENDIF ELSE BEGIN


;           ---------------------------------------------------------------
;           Yes. Check COUNT, OFFSET, and STRIDE vectors for this variable.
;           ---------------------------------------------------------------

            result = check_vectors( ncdf_id, $         ; Input
                                    variable_info, $   ; Input
                                    count, $           ; Input
                                    offset, $          ; Input
                                    stride, $          ; Input
                                    count_vector, $    ; Output
                                    offset_vector, $   ; Output
                                    stride_vector )    ; Output

            IF ( result NE 1 ) THEN BEGIN
              NCDF_CLOSE, ncdf_id
              CATCH, /CANCEL
              RETURN, -1
            ENDIF


;           ----------------------
;           Read the variable data
;           ----------------------

            NCDF_VARGET, ncdf_id, $                 ; Input
                         variable_id, $             ; Input
                         variable_data, $           ; Output
                         count  = count_vector, $   ; Input keyword
                         offset = offset_vector, $  ; Input keyword
                         stride = stride_vector     ; Input keyword

          ENDELSE      ; Scalar or array? Variable dimension IF statement


;         ---------------------------------------------------------------
;         If necessary and required, convert BYTE variable data to STRING
;         ---------------------------------------------------------------

          IF ( FLOAT( !VERSION.RELEASE ) LT fixed_IDL_version  AND $    ; IDL version that differentiates BYTE and STRING NetCDF variables
               STRUPCASE( variable_info.datatype ) EQ 'BYTE' AND $      ; Is the data a BYTE type?
               ( NOT KEYWORD_SET( no_var_byte_to_string ) ) ) THEN $    ; Is the conversion keyword set?
            variable_data = STRING( variable_data )


;         -------------------------------------------
;         Determine the number of variable attributes
;         -------------------------------------------

          n_variable_attributes = variable_info.NATTS


;         --------------------------------------------
;         Retrieve the variable attributes if required
;         --------------------------------------------

          IF ( KEYWORD_SET( variable_attributes ) AND $
               n_variable_attributes GT 0 ) THEN BEGIN


;           ----------------------------------
;           Create the variable data structure
;           ----------------------------------

            IF ( n_variables GT 1 ) THEN BEGIN

;             -- Create variable data structure with generic DATA tag
              variable_data = CREATE_STRUCT( 'data', $        ; Input
                                             variable_data )  ; Input

            ENDIF ELSE BEGIN

;             -- Only this variable being read, so use name as structure tag
              variable_data = CREATE_STRUCT( variable_name, $ ; Input
                                             variable_data )  ; Input

            ENDELSE


;           --------------------------------------
;           loop over current variable's attribute
;           --------------------------------------

            FOR j = 0, n_variable_attributes - 1 DO BEGIN

;             -- Get the current attribute name
              attribute_name = NCDF_ATTNAME( ncdf_id, $       ; Input
                                             variable_id, $   ; Input
                                             j )              ; Input

;             -- Get the current attribute value
              NCDF_ATTGET, ncdf_id, $         ; Input
                           variable_id, $     ; Input
                           attribute_name, $  ; Input
                           attribute          ; Output

;             -- Get the current attribute info
              attribute_info = NCDF_ATTINQ( ncdf_id, $        ; Input
                                            variable_id, $    ; Input
                                            attribute_name )  ; Input

;             -- If necessary and required, convert BYTE attribute to STRING
              IF ( FLOAT( !VERSION.RELEASE ) LT fixed_IDL_version  AND $    ; IDL version that differentiates BYTE and STRING NetCDF variables
                   STRUPCASE( attribute_info.datatype ) EQ 'BYTE' AND $     ; Is the data a BYTE type?
                   ( NOT KEYWORD_SET( no_att_byte_to_string ) ) ) THEN $    ; Is the conversion keyword set?
                attribute = STRING( attribute )

;             -- Add current attribute to variable structure
              variable_data = CREATE_STRUCT( variable_data, $   ; Input
                                             attribute_name, $  ; Input
                                             attribute )        ; Input

            ENDFOR      ; Loop over current variable attributes

          ENDIF       ; Get attributes IF statement


;         -------------------------------
;         Append data to return structure
;         -------------------------------

          IF ( i GT 0 ) THEN BEGIN

;           -- Append data to existing structure
            data = CREATE_STRUCT( data, $                       ; Input
                                  variable_name, $              ; Input
                                  TEMPORARY( variable_data ) )  ; Input

          ENDIF ELSE BEGIN

;           -- Create new structure
            data = CREATE_STRUCT( variable_name, $              ; Input
                                  TEMPORARY( variable_data ) )  ; Input

          ENDELSE

        ENDFOR      ; Loop over requested variables


;     ---------------
;     No data to read
;     ---------------

      ENDIF ELSE BEGIN

        MESSAGE, 'No variables to read!', /INFO

      ENDELSE     ; n_variables > 0 IF statement

    END         ; Data read CASE select.


;   --------------
;   Default action
;   --------------

    ELSE: BEGIN

      dummy = TEMPORARY( data )
      CATCH, /CANCEL
      NCDF_CLOSE, ncdf_id
      MESSAGE, 'Invalid read_data flag!', /INFO
      RETURN, -1

    END

  ENDCASE



;------------------------------------------------------------------------------
;                       -- Close the NetCDF data file --
;------------------------------------------------------------------------------

  NCDF_CLOSE, ncdf_id



;------------------------------------------------------------------------------
;                                  -- Done --
;------------------------------------------------------------------------------

  CATCH, /CANCEL
  RETURN, 1


END


;==============================================================================
; CVS/RCS keyword modification history:
;
; $Log: read_ncdf.pro,v $
; Revision 2.1  1999/10/08 16:45:02  paulv
; - New version that reads attribute data also.
; - Array subsample reads possible using COUNT, OFFSET, and STRIDE
;   keywords.
; - Return data structure is built on the fly rather than building the
;   command statement to create the structure. This should avoid memory
;   problems experienced with the previous version.
;
; Revision 1.3  1999/04/27 18:19:41  paulv
; - Data read from NetCDF file is now returned in the argument list. The
;   function return value is now a status flag only.
; - The NetCDF file is now closed before returning when a read error occurs.
;
; Revision 1.2  1999/04/19 19:39:40  paulv
; Updated header documentation
;
; Revision 1.1  1999/04/19 16:17:41  paulv
; Renamed from read_cdf to read_ncdf.
;
;==============================================================================
