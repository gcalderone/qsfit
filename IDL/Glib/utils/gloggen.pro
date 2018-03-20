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
;  gloggen
;
;PURPOSE:
;  Generate a sequence of number whose logarithm is evenly spaced.
;
;PARAMETERS:
;  _MIN (input, either a 1 ir 2 element array of numbers)
;    First number in the sequence or [first, last] numbers in the
;    sequence.
;
;  _MAX (input, a scalar number)
;    Last number in the sequence or number of elements in output
;    array.
;
;  _NUM (optional input)
;    number of elements in output array.
;
;RETURN VALUE: (array of numbers)
;  Numerical sequence.
;
;SEE ALSO:
;  glib/utils/ggen
;  glib/utils/ggen_delta
;
FUNCTION gloggen, _min, _max, _num
  COMPILE_OPT IDL2
  ON_ERROR, !glib.on_error

  IF (N_PARAMS() EQ 3) THEN BEGIN
     min = _min
     max = _max
     num = _num
  ENDIF $
  ELSE BEGIN
     min = _min[0]
     max = _min[1]
     num = _max[0]     
  ENDELSE

  ret = 10.^(ggen(ALOG10(min), ALOG10(max), num))

  RETURN, ret
END
