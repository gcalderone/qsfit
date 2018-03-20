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
;  gsearch
;
;PURPOSE:
;  Search for any "true" element in the input array.  This
;  function is very similar to the  WHERE built-in function.
;
;PARAMETERS:
;  MAP (input, array of "true"/"false" values)
;    Each element in the array should be evaluable as true or false,
;    in the same way as the WHERE function does. This argument has the
;    same meaning as the first parameter of the WHERE function.
;
;  INDEX (output, array of integer numbers)
;    Index of "true" elements in the array.
;
;  COUNT= (output, an integer number)
;    How may "true" values have been found.
;
;RETURN VALUE (an integer number)
;  1 if at least one "true" value has been found, 0 otherwise.
;
FUNCTION gsearch, map, index, count=count
  COMPILE_OPT IDL2
  ON_ERROR, !glib.on_error

  ;:Handle the case of empty input array
  IF (gn(map) EQ 0) THEN BEGIN
     index = []
     count = 0
  ENDIF $
  ELSE $
     index = WHERE(map, count, /null)

  IF (count GE 1) THEN RETURN, 1
  RETURN, 0
END
