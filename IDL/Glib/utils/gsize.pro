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
;  gsize
;
;PURPOSE:
;  Return the size of input array
;
;PARAMETERS:
;  v (input, array of any type).
;
;RETURN VALUE: (aaray of integer numbers)
;  An array with as many elements as dimensions in input array.
;
FUNCTION gsize, v
  COMPILE_OPT IDL2
  ON_ERROR, !glib.on_error

  IF (gn(v) EQ 1) THEN RETURN, 1
  s = SIZE(v)
  RETURN, s[1:s[0]]
END
