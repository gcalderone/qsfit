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
;  ggen_delta
;
;PURPOSE:
;  Generate a evenly spaced number sequence.
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
;  _delta (input, a number)
;    Interval between two consecutive numbers in the sequence.
;
;RETURN VALUE: (array of numbers)
;  Numerical sequence.
;
;NOTES:
;  The last number in the sequence will be greater than _max if
;  (_max-_min) is not an integer multiple of _delta.
;
;SEE ALSO:
;  glib/utils/ggen
;  glib/utils/gloggen
;
FUNCTION ggen_delta, _min, _max, _delta
  COMPILE_OPT IDL2
  ON_ERROR, !glib.on_error
 
  IF (N_PARAMS() EQ 3) THEN BEGIN
     min = _min
     max = _max
     delta = _delta
  ENDIF $
  ELSE BEGIN
     min = _min[0]
     max = _min[1]
     delta = _max[0]     
  ENDELSE

  ret = min
  REPEAT ret = [ret, ret[-1]+delta] $
  UNTIL (ret[-1] GT max-delta/2.)

  RETURN, ret
END
