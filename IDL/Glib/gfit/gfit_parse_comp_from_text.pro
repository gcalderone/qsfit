; *******************************************************************
; Copyright (C) 2016-2018 Giorgio Calderone
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
;  gfit_add_comp_list
;
;PURPOSE:
;  Add new component(s) to the GFIT model from a file
;
;DESCRIPTION:
;
;PARAMETERS:
;
;NOTES:
;
FUNCTION gfit_parse_comp_from_text, comp, text, FILE=file
  COMPILE_OPT IDL2
  ON_ERROR, !glib.on_error

  IF (KEYWORD_SET(file)) THEN BEGIN
     text = []
     OPENR, lun, file, /get_lun
     WHILE (~EOF(lun)) DO BEGIN
        l = ''
        READF, lun, l
        text = [text, STRTRIM(l, 2)]
     ENDWHILE
     FREE_LUN, lun
  ENDIF
  
  templates = HASH()
  str = { name: '', $
          type: '', $
          comp: comp }
  out = LIST()
  
  columns = []
  FOR itext=0, gn(text)-1 DO BEGIN
     l = text[itext]
     l = STRTRIM(l, 2)
     IF (l EQ '') THEN CONTINUE
     IF (STRMID(l, 0, 1) EQ '#') THEN CONTINUE ;;Comment
     l = STRTRIM(STRSPLIT(l, '|', /PRESERVE_NULL, /EXTRACT), 2)

     IF (gn(columns) EQ 0) THEN BEGIN
        columns = l
        IF (columns[0] NE "Name") THEN MESSAGE, 'First column must be "Name"'
        parnames = l[1:-1]
     ENDIF $
     ELSE BEGIN
        newname = STRTRIM(STRSPLIT(l[0], ':', /PRESERVE_NULL, /EXTRACT), 2)
        type = ""
        IF (gn(newname) EQ 2) THEN type = newname[1]
        newname = newname[0]
        l = l[1:-1]

        IF (templates.haskey(type)) THEN newcomp = templates[type] $
        ELSE                             newcomp = comp

        FOR ipar=0, gn(parnames)-1 DO BEGIN
           IF (l[ipar] EQ '') THEN CONTINUE
           d = STRTRIM(STRSPLIT(l[ipar], ';', /PRESERVE_NULL, /EXTRACT), 2)
           FOR j=0, gn(d)-1 DO BEGIN
              IF (parnames[ipar] NE "") THEN BEGIN
                 e = ""
                 IF d[j] NE "" THEN BEGIN
                    e = "newcomp.par." + parnames[ipar] + "."
                    CASE j OF
                       0: e += "val = FLOAT(" + d[j] + ")"
                       1: e += "fixed = LONG(" + d[j] + ")"
                       2: e += "limits = FLOAT(" + d[j] + ")"
                       3: e += "expr = """ + d[j] + """"
                    ENDCASE
                 ENDIF
              ENDIF $
              ELSE BEGIN
                 e = "newcomp." + d[j]
              ENDELSE
              IF (e NE "") THEN BEGIN
                 PRINT, e
                 IF (~EXECUTE(e)) THEN $
                    MESSAGE, 'An error occurred while executing: ' + e
              ENDIF
           ENDFOR
        ENDFOR

        IF (newname EQ "") THEN templates[type] = newcomp $  ;; Template
        ELSE BEGIN  ;; New component
           str.name = newname
           str.type = type
           str.comp = newcomp
           out.add, str
        ENDELSE
     ENDELSE
  ENDFOR

  out = out.toArray()
  RETURN, out
END
