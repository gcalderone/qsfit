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
;  gstru_insert
;
;PURPOSE:
;  Insert a field at a given position in a structure.
;
FUNCTION $
   gstru_insert        $ ;;RET|Structure with newly inserted field
   , stru              $ ;;IN |Input structure
   , name              $ ;;IN |Name of new field (must be a scalar string)
   , field             $ ;;IN |The new field to be added. May be a scalar or array of any type (see AS_ARRAY keyword)
   , position          $ ;;OPT|Integer specifying the position at which the new field is to be inserted. 0 means at the beginning of the structure.  If not given append the field to the end of the structure.
   , AS_ARRAY=as_array   ;;KW |
  COMPILE_OPT IDL2
  ON_ERROR, !glib.on_error

  as_array = KEYWORD_SET(as_array)
  IF (KEYWORD_SET(as_array)) THEN BEGIN
     toBeAdded = CREATE_STRUCT(name, field)
  ENDIF $
  ELSE BEGIN
     toBeAdded = CREATE_STRUCT(name, field[0])
     toBeAdded = REPLICATE(toBeAdded, gn(field))
     toBeAdded.(0) = field
  ENDELSE

  ;;Add on an empty structure
  IF (gn(stru) EQ 0) THEN $
     RETURN, toBeAdded

  ;;Set position if not given
  IF (gn(position) EQ 0) THEN $
     position = N_TAGS(stru)  ;;append to struct

  ;;Check
  IF (position GT N_TAGS(stru)) THEN MESSAGE, 'Invalid position: ' + gn2s(position)
  ;;IF (~as_array   AND   $
  ;;  (gn(stru) NE gn(field)))   THEN MESSAGE, 'Incompatible size'

  ;;Field names
  names = TAG_NAMES(stru)

  ;;Fields before new one
  pre = []
  IF (position GT 0) THEN $
     pre = gstru_sub(stru[0], keep=names[0:position-1])

  ;;Fields after new one
  post = []
  IF (position LT N_TAGS(stru)) THEN $
     post = gstru_sub(stru[0], keep=names[position:*])

  ;;Final structure
  final = CREATE_STRUCT(pre, toBeAdded[0], post)
  final = REPLICATE(final, gn(stru))
  STRUCT_ASSIGN, stru, final

  ;;Assign new values
  final.(N_TAGS(pre)) = toBeAdded.(0)

  RETURN, final
END
