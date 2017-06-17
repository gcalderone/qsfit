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
;  gn2base
;
;PURPOSE:
;  Convert a decimal number into another base.
;
;PARAMETERS:
;  IN (input, a scalar or array of integer numbers)
;    Input integer number(s)
;
;  BASE (input, an integer number greater than or equal to 2)
;    Output base
;
;  LENGTH= (optional input, an integer number)
;    Minimum output length of the array (padded with zero).  If the IN
;    variable is an array this keyword must be given.
;
;RETURN VALUE:
;  Array of integer numbers with the desired base representation of
;  input number.
;
FUNCTION gn2base, in, base, LENGTH=length
  COMPILE_OPT IDL2
  ON_ERROR, !glib.on_error

  IF (gn(in) GT 1) THEN BEGIN
     IF (gn(length) EQ 0) THEN $
        MESSAGE, 'The LENGTH keyword must be given when passing an array'
     
     ret = LONARR(gn(in), length)
     FOR i=0, gn(in)-1 DO $
        ret[i,*] = gn2base(REFORM(in[i]), base, LENGTH=length)
     RETURN, ret
  ENDIF

  n = ULONG(in)
  IF (n EQ 0) THEN RETURN, 0

  ret = []
  WHILE (n GT 0) DO BEGIN
     ret = [ret, n MOD base]
     n /= base
  ENDWHILE

  IF (gn(length) EQ 1) THEN BEGIN
     IF (gn(ret) LT length) THEN $
        ret = [ret, REPLICATE(0, length-gn(ret))] $
     ELSE $
        MESSAGE, 'Array of LENGTH='+gn2s(length) + ' is too short.'
  ENDIF

  RETURN, REVERSE(BYTE(ret))
END
