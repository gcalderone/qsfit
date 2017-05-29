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
;  gtype
;
;PURPOSE:
;  Replacement to SIZE(/dim)
;
;PARAMETERS:
;  A (input, array of any type)
;  Array whose size of each dimension is being queried.
;
;RETURN VALUE: (array of integer numbers)
;  Number of elements in each dimension of input array.
;
FUNCTION gdim, a
  COMPILE_OPT IDL2
  ON_ERROR, !glib.on_error

  size = size(a, /dim)

  IF ((gn(size) EQ 1)   AND   (size[0] EQ 0)) THEN $
     size = 1

  RETURN, size
END
