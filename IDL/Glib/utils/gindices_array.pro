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
;  gindices_array
;
;PURPOSE:
;  Converts an array multi-dimensional subscripts into corresponding
;  one-dimensional subscript.  This function is the inverse of
;  ARRAY_INDICES.
;
;PARAMETERS:
;  A (input, an array of any type)
;    An array whose dimensions should be used in converting the
;    subscripts. If the DIMENSION keyword is set the array should be
;    a 1D array of integer numbers containing the dimensions.
;
;  INDEX (input, multi dimensional array of integer numbers)
;   Multi-dimensional subscripts of input array.
;
;  /DIMENSION (keyword)
;    If set the input array contains the size of each dimension.
;
;RETURN VALUE: (array of integer numbers)
;  One-dimensional subscript.
;
FUNCTION gindices_array, a, index, DIMENSION=dimension
  COMPILE_OPT IDL2
  ON_ERROR, !glib.on_error

  IF (NOT KEYWORD_SET(dimension)) THEN $
     dims = size(a, /DIM) $
  ELSE $
     dims = a

  IF (N_ELEMENTS(dims) NE N_ELEMENTS(index)) THEN $
     MESSAGE, 'Wrong input parameters'

  FOR i=1, N_ELEMENTS(dims)-2 DO $
     dims[i] *= dims[i-1]
  dims[N_ELEMENTS(dims)-1] = 1
  dims = SHIFT(dims, 1)
  
  RETURN, TRANSPOSE(dims) # index
END





