; *******************************************************************
; Copyright (C) 2016 Giorgio Calderone
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
;  gn2s
;
;PURPOSE:
;  Convert a number into a string, dropping non-significant zeroes and
;  blanks.
;
;PARAMETERS:
;  D (input, a scalar or array of numbers)
;   Numbers to be converted to strings.
;
;  FMT (optional input, a string)
;    Printf-style format (must include the % character).
;
;  NAN= (optional input, a string)
;    String representation of NaN numbers (default: "NaN").
;
;  /FAST (keyword)
;    Dropping non-significant zeroes may be a time consuming task.
;    With this keyword it is disabled.
;
;RETURN VALUE (a scalar or array of strings)
;  String(s) with number representation of input data
;
FUNCTION gn2s, d, fmt, NAN=nan, FAST=fast
  COMPILE_OPT IDL2
  ON_ERROR, !glib.on_error

  ;;Default NaN representation
  IF (gn(nan) EQ 0) THEN nan = 'NaN'

  ;;Format
  IF (N_PARAMS() EQ 1) THEN BEGIN
     IF (gtype(d[0], /int)) THEN fmt = '%d'     $
     ELSE                        fmt = '%11.5g'
  ENDIF

  ;;Cast number to a string
  s = STRING(FORMAT=gcfmt(fmt), d)

  IF (gn(d) GT 1) THEN $
     s = REFORM(s, gdim(d))

  IF (N_PARAMS() EQ 1) THEN $
     s = STRTRIM(s, 2)

  i = WHERE(~FINITE(d))
  IF (i[0] NE -1) THEN $
     s[i] = nan

  
  IF (~KEYWORD_SET(fast)   AND   (N_PARAMS() EQ 1)) THEN BEGIN
     IF (gtype(d[0], /float)) THEN BEGIN

        ;;Drop non significant digits
        FOR i=0, gn(d)-1 DO BEGIN
           b = BYTE(s[i])
           
           iDot = WHERE(b EQ 46)
           IF (iDot[0] EQ -1) THEN CONTINUE
           iDot = iDot[0]
           
           iExp = WHERE(b EQ 69  OR  b EQ 101)
           IF (iExp[0] NE -1) THEN CONTINUE
           
           FOR j=N_ELEMENTS(b)-1, iDot, -1 DO BEGIN
              IF (b[j] NE 48  AND  b[j] NE 46) THEN BREAK
              b[j] = 0
           ENDFOR
           
           b = b[WHERE(b NE 0)]
           s[i] = STRING(b)


           ;;IF (STRPOS(STRUPCASE(s[i]), 'E') EQ -1) THEN BEGIN
           ;;   WHILE (1) DO BEGIN
           ;;      c = STRMID(s[i], 0, 1, /rev)
           ;;      IF (c NE '0'   AND   c NE '.') THEN BREAK
           ;;      s[i] = STRMID(s[i], 0, STRLEN(s[i])-1)
           ;;      IF (c EQ '.') THEN BREAK
           ;;   ENDWHILE
           ;;ENDIF
        ENDFOR
     ENDIF
  ENDIF

  RETURN, s
END
