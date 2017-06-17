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
;  gstru_tagnames
;
;PURPOSE:
;  Return the tag names of a structure.  If the structure is a nested
;  one it also returns all nested tag names, separated by a dot.
;
;PARAMETERS:
;  STR (input, a scalar or array of structure)
;    Input structure.
;
;RETURN VALUE: (array of string)
;  the tag names of the structure.
;
FUNCTION gstru_tagnames, str
  COMPILE_OPT IDL2
  ON_ERROR, !glib.on_error

  tagn = TAG_NAMES(str)

  ret = []
  FOR i=0, gn(tagn)-1 DO BEGIN
     IF (gtype(str.(i)) EQ 'STRUCT') THEN $
        ret = [ret, tagn[i] + '.' + gstru_tagnames(str.(i))] $
     ELSE $
        ret = [ret, tagn[i]]
  ENDFOR

  RETURN, ret
END

