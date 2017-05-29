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
;  gproduct
;
;PURPOSE:
;  Return product of elements of a numeric array
;
;PARAMETERS:
;  A (input, array of numbers)
;
;RETURN VALUE (a scalar number):
;  The product of all elements in input array.
;
FUNCTION gproduct, a
  COMPILE_OPT IDL2
  ON_ERROR, !glib.on_error

  ret = a[0]
  FOR i=1l, N_ELEMENTS(a)-1 DO $
     ret *= a[i]

  RETURN, ret
END
