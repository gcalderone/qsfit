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
;  gfloat
;
;PURPOSE:
;  Convert input string into a float.  If the conversion cannot be performed returns NaN.
;
;PARAMETERS:
;  S (input, a scalar or array of strings)
;    The string to convert to floating point numbers
;
;  /DOUBLE (keyword)
;    Use double instead of single precision float numbers.
;
;RETURN VALUE: (array of floating point numbers)
;  Numbers converted to floating point.
;
;NOTES:
;  If an input string can not be converted to a number NaN is
;  returned.
;
FUNCTION gfloat, s, DOUBLE=double

  COMPILE_OPT IDL2
  CATCH, Error_status
  IF Error_status NE 0 THEN BEGIN
     CATCH, /cancel
     IF (double) THEN RETURN, !VALUES.D_NAN  $
     ELSE             RETURN, !VALUES.F_NAN
  ENDIF

  double = KEYWORD_SET(double)

  IF (gn(s) GT 1) THEN BEGIN
     ret = REPLICATE((double   ?   0.d   :   0.), gsize(s))
     FOR i=0, gn(s)-1 DO $
        ret[i] = gfloat(s[i], DOUBLE=double)

     RETURN, ret
  ENDIF


  IF (double) THEN ret = 0.d $
  ELSE             ret = 0.

  READS, s, ret

  RETURN, ret
END
