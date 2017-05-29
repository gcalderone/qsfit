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
;  gstru_flatten
;
;PURPOSE:
;  Return a "flattened" structure, i.e. a structure whose
;  substructures are flattened.
;
;PARAMETERS:
;  STR (input, a scalar or array of structure)
;    Input structure.
;
;RETURN VALUE: (a scalar or array of structure)
;  The "flattened" structure.
;
FUNCTION gstru_flatten, str
  COMPILE_OPT IDL2
  ON_ERROR, !glib.on_error

  tagn = gstru_tagnames(str)

  tmp = tagn
  FOR i=0, gn(tagn)-1 DO BEGIN
     tmp[i] = STRJOIN(STRSPLIT(tagn[i], '.', /extract), '__')
  ENDFOR
  tmp = '{ ' + STRJOIN(tmp + ': str.' + tagn, ', ') + ' }'
  
  IF (~EXECUTE('ret = ' +tmp)) THEN $
     MESSAGE, 'Error while creating return structure'

  RETURN, ret
END
