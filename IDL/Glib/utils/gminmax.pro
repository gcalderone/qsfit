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
;  gminmax
;
;PURPOSE:
;  Return an array with minimum and maximum value of an aray
;
;PARAMETERS:
;  A (input, array of numbers)
;    Input array.
;
;  DIMEN= (optional input, a scalar integer number)
;    Dimension over which the search is to be performed (equivalent to
;    the DIMENSION keyword of the MIN function).
;
;RETURN VALUE: (2 elements array of integer numbers)
;  Index of minimum and maximum value in the array
;
FUNCTION gminmax, a, DIMEN=dimen
  COMPILE_OPT IDL2
  ON_ERROR, !glib.on_error

  min = MIN(a[WHERE(FINITE(a))], MAX=max, DIMEN=dimen)

  IF (KEYWORD_SET(dimen)) THEN $
     RETURN, TRANSPOSE([[min], [max]])

  RETURN, [min, max]
END
