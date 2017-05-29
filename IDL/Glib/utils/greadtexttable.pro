; *******************************************************************
; Copyright (C) 2016,2017 Giorgio Calderone
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
;  greadtexttable
;
;PURPOSE:
;  Read an table (i.e. an array of structures) written in a text file.
;
;NOTES:
;  Empty lines and lines beginning with a "#" are considered comments
;  and skipped.
;
;PARAMETERS:
;  FILE (input, a scalar string)
;    Input file name
;
;  SEP (input, a scalar string)
;    Field separator in the text file (default: tab)
;
;  /DROPNULL (keyword)
;    If given two consecutive separators are considered as just one
;    separator.
;
;  TEMPLATE= (optional input, a scalar structure)
;    The template structure to be filled with data upon returning.
;
;RETURN VALUE:
;  An array of structures with as many fields as the columns in the
;  file and as many rows as the lines. Each field in the structure is
;  a STRING.
;
FUNCTION greadtexttable, file, sep, DROPNULL=dropnull, TEMPLATE=template
  COMPILE_OPT IDL2
  ON_ERROR, !glib.on_error

  IF (N_PARAMS() EQ 1) THEN sep = STRING(9b) ;;Default: TAB

  nfld = []

  ;;We will read the whole file two times
  FOR mode=0, 1 DO BEGIN
     nrow = 0

     OPENR, lun, file, /GET_LUN
     WHILE NOT EOF(lun) DO BEGIN
        s = ''
        READF, lun, s
        IF (STRTRIM(s, 2) EQ '') THEN CONTINUE
        IF (STRMID(s, 0, 1) EQ '#') THEN CONTINUE ;;Comment

        array = STRSPLIT(s, sep, PRESERVE_NULL=~KEYWORD_SET(dropnull), /EXTRACT)
        n = gn(array)

        CASE (mode) OF 
           0: BEGIN
              IF (gn(nfld) EQ 0) THEN nfld = n $
              ELSE BEGIN
                 IF (n NE nfld) THEN BEGIN
                    HELP, nfld
                    PRINT, s
                    MESSAGE, 'Number of field varies among lines'
                 ENDIF
              ENDELSE

           END
           1: BEGIN
              FOR i=0, nfld-1 DO $
                 str[nrow].(i) = STRTRIM(array[i], 2)
           END
        ENDCASE

        nrow += 1
     ENDWHILE
     FREE_LUN, lun


     IF (mode EQ 0) THEN BEGIN
        str = []
        FOR i=0, nfld-1 DO $
           str = CREATE_STRUCT(str, 'F' + gn2s(i), '')
        str = REPLICATE(str, nrow)
     ENDIF
  ENDFOR

  ret = str
  IF (KEYWORD_SET(template)) THEN BEGIN
     IF (N_TAGS(str) NE N_TAGS(template)) THEN $
        MESSAGE, 'Output and template structure has different number of tags ' + $
                 ' (output=' + gn2s(N_TAGS(str)) + ', template=' + gn2s(N_TAGS(template)) + ')'

     ret = REPLICATE(template[0], gn(str))
     FOR i=0, N_TAGS(str)-1 DO BEGIN
        IF (gtype(ret.(i)[0]) EQ 'STRING') THEN $
           ret.(i) = str.(i) $
        ELSE IF (gtype(ret.(i)[0], /integer)) THEN $
           ret.(i) = str.(i) $
        ELSE IF (gtype(ret.(i)[0], /float)) THEN $
           ret.(i) = gfloat(str.(i)) $
        ELSE $
           MESSAGE, 'The tags in template structure must be either strings, or integer, or floating point numbers' 
     ENDFOR
  ENDIF

  RETURN, ret
END
