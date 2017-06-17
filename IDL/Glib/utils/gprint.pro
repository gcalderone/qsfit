; *******************************************************************
; Copyright (C) 2016-2017 Giorgio Calderone
;
; This program is free software; you can redistribute it and/or
; modify it under the terms of the GNU General Public icense
; as published by the Free Software Foundation; either version 2
; of the License, or (at your option) any later version.
;
; This program is distributed in the hope that it will be useful,
; but WITHOUT ANY WARRANTY; without even the implied warranty of
; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
; GNU General Public License for more details.
;
; You should have received a copy of the GNU General Public License
; along with this program. If not, see <http://www.gnu.org/licenses/>.
;
; *******************************************************************

;=====================================================================
;NAME:
;  gprint
;
;PURPOSE:
;  Replacement for PRINT built-in procedure to write messages on both
;  standard output and a file with a single statement.
;
;DESCRIPTION:
;  This routine provides a convenient way to write log messages.  The
;  syntax is very similar to the built-in PRINT procedure, hence
;  "gprint" can easily replace it in IDL code.
;
;  Its standard behaviour is to print the messages on standard output.
;  Such behaviour can be modified using the "gprint_mgr" procedure.
;  In particular it is possible to open a file for writing, and write
;  on both standard output and the file with a single call to "gprint"
;  (see gprint_mgr.pro for further information).
;
;  Also, gprint has a peculiar behaviour when arguments are arrays:
;  all of them must have the same number of elements, so that they can
;  br printed in a tabular form.
;
;LIMITATIONS:
;  gprint as a limited number of arguments, currently 9, hence it can
;  be used as a replacement for PRINT as long as at most 9 arguments
;  are provided.  Also, gprint forwards only the FORMAT keyword to
;  PRINT, the other keyords accepted by PRINT are not available.
;
;PARAMETERS:
;  S0, S1, etc..., (input, scalar or arrays of any type except
;  structure, up to 13 arguments)
;    Data to be printed.
;
;  FORMAT= (optional input, a scalar string)
;    Forwarded to PRINT/PRINTF.
;
;  CFORMAT= (optional input, a scalar string)
;    A C-style format specification.  It is forwarded to PRINT/PRINTF
;    through gcfmt().
;
;  /OVERRIDE (keyword)
;    Override current settings and print on both standard output and
;    opened file (if any).
;
;  LUN= (optional input, logical unit number)
;    Print on this LUN instead of the file opened by gprint_mgr. Also
;    suppress printing on standard output.
;
;SEE ASO:
;  utils/gprint_mgr.pro
;
PRO gprint, s1, s2, s3, s4, s5, s6, s7, s8, s9 $
            , FORMAT=format, CFORMAT=cformat, OVERRIDE=override, LUN=ext_lun
  COMMON COM_GPRINT_MGR, use_stdout, use_lun, current_lun
  COMPILE_OPT IDL2
  ON_ERROR, !glib.on_error

  ;;Initialize common variables
  IF (N_ELEMENTS(current_lun) EQ 0) THEN gprint_mgr

  IF (N_PARAMS() GT 9) THEN $
     MESSAGE, 'At most 9 arguments are supported by gprint, while ' + gn2s(N_PARAMS()) + ' were given'

  IF (KEYWORD_SET(cformat)) THEN $
     format = gcfmt(cformat)

  IF (KEYWORD_SET(ext_lun)) THEN $
     lun_list = ext_lun $
  ELSE BEGIN
     lun_list = []
     IF (use_stdout   OR   KEYWORD_SET(override)) THEN $
        lun_list = [lun_list, -1]

     IF (current_lun NE -1) THEN BEGIN
        IF (use_lun   OR   KEYWORD_SET(override)) THEN $
           lun_list = [lun_list, current_lun]
     ENDIF
  ENDELSE

  IF (N_PARAMS() EQ 0) THEN s1=''

  ;;Call PRINTF with the appropriate number of arguments 
  FOREACH lun, lun_list DO BEGIN
     FOR i=0, N_ELEMENTS(s1)-1 DO BEGIN
        CASE (N_PARAMS()) OF
           0: PRINTF, format=format, lun, ''
           1: PRINTF, format=format, lun, s1[i]
           2: PRINTF, format=format, lun, s1[i], s2[i]
           3: PRINTF, format=format, lun, s1[i], s2[i], s3[i]
           4: PRINTF, format=format, lun, s1[i], s2[i], s3[i], s4[i]
           5: PRINTF, format=format, lun, s1[i], s2[i], s3[i], s4[i], s5[i]
           6: PRINTF, format=format, lun, s1[i], s2[i], s3[i], s4[i], s5[i], s6[i]
           7: PRINTF, format=format, lun, s1[i], s2[i], s3[i], s4[i], s5[i], s6[i], s7[i]
           8: PRINTF, format=format, lun, s1[i], s2[i], s3[i], s4[i], s5[i], s6[i], s7[i], s8[i]
           9: PRINTF, format=format, lun, s1[i], s2[i], s3[i], s4[i], s5[i], s6[i], s7[i], s8[i], s9[i]
        ENDCASE
     ENDFOR
  ENDFOREACH
END


;;PRO gprint, s0, s1, s2, s3, s4, s5, s6, s7, s8, s9, s10, s11, s12       $
;;            , FORMAT=format, CFORMAT=cformat, OVERRIDE=override, LUN=lun
;;  COMMON COM_GPRINT_MGR, use_stdout, use_lun, current_lun
;;  COMPILE_OPT IDL2
;;  ON_ERROR, !glib.on_error
;;
;;  ;;Initialize common variables
;;  IF (N_ELEMENTS(current_lun) EQ 0) THEN gprint_mgr
;;
;;  ;;Prepare print argument
;;  IF (N_PARAMS() GT 0) THEN BEGIN
;;     print_arg = STRARR(N_ELEMENTS(s0))
;;     FOR i=0, N_ELEMENTS(s0)-1 DO $
;;        print_arg[i] += STRJOIN('s' + STRTRIM(STRING(LINDGEN(N_PARAMS())), 2) + '[' + gn2s(i) + ']', ', ')
;;  ENDIF $
;;  ELSE $
;;     print_arg = '""'
;;
;;  IF (KEYWORD_SET(cformat)) THEN $
;;     format = gcfmt(cformat)
;;
;;  IF (KEYWORD_SET(format)) THEN $
;;     print_arg += ', FORMAT=format'
;;
;;  ;;Call PRINT on both standard output and output file (if any)
;;  FOR i=0, N_ELEMENTS(print_arg)-1 DO BEGIN
;;     IF (KEYWORD_SET(lun)) THEN BEGIN
;;        gassert, EXECUTE('PRINTF, lun, ' + print_arg[i])
;;     ENDIF $
;;     ELSE BEGIN
;;        IF (use_stdout   OR   KEYWORD_SET(override)) THEN $
;;           gassert, EXECUTE('PRINT, ' + print_arg[i])
;;
;;        IF (current_lun NE -1) THEN BEGIN
;;           IF (use_lun   OR   KEYWORD_SET(override)) THEN $
;;              gassert, EXECUTE('PRINTF, current_lun, ' + print_arg[i])
;;        ENDIF
;;     ENDELSE
;;  ENDFOR
;;END
