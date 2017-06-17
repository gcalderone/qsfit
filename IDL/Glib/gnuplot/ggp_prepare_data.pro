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
;  ggp_prepare_data
;
;PURPOSE:
;  Convert 1D or multi-dimensional arrays of data into a tabular form
;  suitable to be passed to gnuplot for plotting.
;
;DESCRIPTION:
;  This function performs a re-arrangement of the input array(s),
;  either 1D or multi dimensional ones, to produce data in a tabular
;  form suitable to be passed to gnuplot for plotting.  The output
;  table is returned as an array of structures, with as many elements
;  as rows in the table and as many fields as the column in the table.
;
;  Since each plot requires both independent and dependent variables,
;  this function automatically adds the independent variables if not
;  given as input.  Also, in the multi-dimensional case, this function
;  automatically repeat the independent variables to obtain a 1D
;  representation of input data.  Finally, it tells whether the data
;  in the output table can be logically separated in groups of
;  consecutive rows.  This groups are separated by the so-called "key
;  break".
;
;  Input data can be of any type, including strings.  The functions
;  accepts up to 20 arguments.
;
;  The output structure is suitable to be printed with gps.  Also the
;  key breaks (returned through the KEYBREAK= keyword), are suitable
;  to be passed to gps via the SKIP= keyword.
;
;  This function is supposed to be called oly by ggp_data and
;  ggp_hist, not by users.
;
;  In the following table we report the input data arrangement (on the
;  left) and the size of the resulting tabular structure (on the
;  right):
;
;        *** INPUT ***                      |                     *** OUTPUT ***
;  a scalar                                 | 1 row, 2 columns (1 column automatically added)
;  a scalar, a scalar                       | 1 row, 2 columns
;  a scalar, a scalar, a scalar             | 1 row, 3 columns
;  ...                                      | ...
;                                           |
;  [1D array of size N]                     | N rows, 2 columns (1 column automatically added)
;  [N], [N]                                 | N rows, 2 columns
;  [N], [N], [N]                            | N rows, 3 columns
;  ...                                      | ...
;                                           |
;  [2D array of size N,M]                   | N*M rows, 3 columns (2 columns automatically added, M-1 key breaks)
;  [N,M], [N,M]                             | N*M rows, 4 columns (2 columns automatically added, M-1 key breaks)
;  [N,M], [N,M], [N,M]                      | N*M rows, 5 columns (2 columns automatically added, M-1 key breaks)
;  ...                                      | ...
;                                           |
;  [N], [M], [N,M]                          | N*M rows, 3 columns (M-1 key breaks)
;  [N], [M], [N,M], [N,M]                   | N*M rows, 4 columns (M-1 key breaks)
;  [N], [M], [N,M], [N,M], [N,M]            | N*M rows, 5 columns (M-1 key breaks)
;                                           |
;  [N,M,P]                                  | N*M*P rows, 4 columns (3 columns automatically added, P-1 key breaks)
;  [N,M,P], [N,M,P]                         | N*M*P rows, 5 columns (3 columns automatically added, P-1 key breaks)
;  [N,M,P], [N,M,P], [N,M,P]                | N*M*P rows, 6 columns (3 columns automatically added, P-1 key breaks)
;  ...                                      | ...
;                                           |
;  [N], [M], [P], [N,M,P]                   | N*M*P rows, 4 columns (P-1 key breaks)
;  [N], [M], [P], [N,M,P], [N,M,P]          | N*M*P rows, 5 columns (P-1 key breaks)
;  [N], [M], [P], [N,M,P], [N,M,P], [N,M,P] | N*M*P rows, 5 columns (P-1 key breaks)
;  ...                                      | ...
;                                           |
;  a scalar, [N>1]                          | ERROR
;  [N], [M!=N]                              | ERROR
;  [N], [N], [M!=N]                         | ERROR
;  [N], [M], [P,Q]  (with N!=P or M!=Q)     | ERROR
;  ...                                      | ...
;
;  The function accepts also a structure as first and only parameter.
;  In this case the fields of the structure are taken as individual
;  parameters.
;
;
;PARAMETERS:
;  P1, P2, etc... (input, array of any type except structure, up to 20
;  arguments).
;    Input data.
;
;  KEYBREAK= (output, array of integer numbers)
;     Locations of keybreaks.  May be passed to gps through the SKIP=
;     keyword.
;
FUNCTION ggp_prepare_data   $
   ,  p1,  p2,  p3,  p4,  p5,  p6,  p7,  p8,  p9, p10  $
   , p11, p12, p13, p14, p15, p16, p17, p18, p19, p20  $
   , KEYBREAK=keybreak
  COMPILE_OPT IDL2
  ON_ERROR, !glib.on_error

  IF (gtype(p1) EQ 'STRUCT') THEN $
     in = p1 $
  ELSE BEGIN
     ll = 'p' + gn2s(INDGEN(N_PARAMS())+1)
     ll += ': REFORM(' + ll + ')'
     ll = '{ ' + STRJOIN(ll, ', ') + ' }'
     dummy = EXECUTE('in = ' + ll)
  ENDELSE

  ;;Collect size and dimensions
  nd = N_TAGS(in)
  ndim = BYTARR(nd)
  size = LONARR(nd)

  FOR i=0, nd-1 DO BEGIN
     size[i] = gn(     in.(i))
     ndim[i] = gn(gdim(in.(i)))

     IF (i GT 0) THEN BEGIN
        IF (ndim[i] LT ndim[i-1]) THEN $
           MESSAGE, 'Independent (1D) data must be given BEFORE dependent (nD) data'
     ENDIF
  ENDFOR


  i = WHERE(ndim GT 1)
  IF (i[0] EQ -1) THEN BEGIN
     ;;All data are 1D
     iind = 0 ;;hence independent variable is just the first
     IF (nd EQ 1) THEN iind = -1 ;;...unless we have only one variable: in this case it is dependent and a mock one will be added below
  ENDIF $
  ELSE BEGIN
     ;;Beyond 1D (independent) variables, only one multidimensional
     ;;arrays are allowed, e.g. only 2D or only 3D etc...
     j = WHERE(ndim GT MIN(ndim[i]))
     IF (j[0] NE -1) THEN $
        MESSAGE, 'Incompatible data dimensions'

     iind = i[0]-1
  ENDELSE

  ;;Variables up to iind index are independent.  iind=-1 means that
  ;;independent (1D) data are missing.  In this case we will add fake
  ;;one(s)
  IF (iind EQ -1) THEN BEGIN
     dim = gdim(in.(0))

     FOR i=gn(dim)-1, 0, -1 DO BEGIN
        in = CREATE_STRUCT('i'+gn2s(i), FINDGEN(dim[i]), in)
        size = [dim[i], size]
        ndim = [1, ndim]
     ENDFOR

     nd  += gn(dim)
     iind = gn(dim)-1
     autoind = 'A'
  ENDIF $
  ELSE  $
     autoind = ''

  ;;Max size
  maxsize = MAX(size)

  ;;SIZE will contain the size of independent variables
  size = size[0:iind]

  ;;Create output structure
  ret = []
  FOR i=0, nd-1 DO $
     ret = CREATE_STRUCT(ret $
                         ,  (i LE iind  ?  autoind+'I'  :  'D') + gn2s(i) $
                         ,  (in.(i))[0])
  ret = REPLICATE(ret, gproduct(size))

  IF (gn(ret) NE maxsize) THEN $
     MESSAGE, 'Not enough independent variable(s)'

  ;;Copy independent variables
  iii = LINDGEN(maxsize)
  FOR j=0, iind DO BEGIN
     nn = 1
     IF (j GT 0) THEN nn = gproduct(size[0:j-1])

     k = (iii / nn) MOD size[j]
     ret.(j) = (in.(j))[k]
  ENDFOR

  FOR j=iind+1, nd-1 DO BEGIN
     ret.(j) = REFORM(in.(j), maxsize)
  ENDFOR

  ;;Map of key breaks
  keyb = BYTARR(gn(ret))

  ;;A key break is a location in which all key field changes
  ;;simultaneously.  I'll check only the last key (iind) since it is
  ;;the slowest changing key.  This apply only if there are at least
  ;;two keys.
  IF (iind GT 0) THEN BEGIN
     lastind = ret.(iind)
     n = gn(lastind)
     keyb[0:n-2] = (lastind[0:n-2] NE lastind[1:n-1])
  ENDIF

  ;;Last record is by definition a key break (so that WHERE never returns -1)
  keyb[-1] = 1
  keybreak = WHERE(keyb)

  RETURN, ret
END
