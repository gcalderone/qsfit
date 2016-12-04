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
;  gn2base
;
;PURPOSE:
;  Convert a decimal number into an hexadecimal string representation.
;
;PARAMETERS:
;  NN (input,  an integer number)
;    Input integer number.
;
;RETURN VALUE: (array of strings)
;  Hexadecimal representation of input number.
;
FUNCTION gn2hex, nn
  COMPILE_OPT IDL2
  ON_ERROR, !glib.on_error

  conv = ['0', '1', '2', '3', '4', '5', '6', '7', '8', '9', 'A', 'B', 'C', 'D', 'E', 'F']

  nn16 = gn2base(nn, 16)

  ret = STRARR(gn(nn16))
  ret = conv[nn16]
  RETURN, ret
END
