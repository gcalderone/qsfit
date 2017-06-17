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
;  gstru_sub
;
;PURPOSE:
;  Return a subset of a structure or an array of structures. Fields
;  can optionally be renamed.
;
;PARAMETERS:
;  STRUCT (input, a scalar or array of structure)
;    Input structure.
;
;  KEEP= (optional input, scalar or array of string)
;    Name of fields to be retained.
;
;  DROP= (optional input, scalar or array of string)
;    Name of fields to be dropped.
;
;  RENAME= (optional input, scalar or array of string)
;    New names of retained fields. An empty string means keep the
;    original name. 
;
;NOTES:
;  Either the KEEP= or DROP= keyword must be given.
;
;RETURN VALUE:
;  a scalar or array of modified structure.
;
FUNCTION gstru_sub, struct $
                    , KEEP=keep, DROP=drop, RENAME=_rename
  COMPILE_OPT IDL2
  ON_ERROR, !glib.on_error  

  ;;Check input keywords
  IF (KEYWORD_SET(keep)  AND  KEYWORD_SET(drop)) THEN $
     MESSAGE, 'The KEEP and DROP keywords are incompatible'

  IF (~KEYWORD_SET(keep)  AND  ~KEYWORD_SET(drop)) THEN $
     MESSAGE, 'No KEEP or DROP keyword provided'

  ;;Get field names
  fields = TAG_NAMES(struct[0])

  ;;Identify fields to be copied in output structure
  IF (KEYWORD_SET(keep)) THEN BEGIN
     search = STRING(keep)
     keep_id = REPLICATE(0, N_ELEMENTS(fields))
  ENDIF $
  ELSE BEGIN
     search = STRING(drop)
     keep_id = REPLICATE(1, N_ELEMENTS(fields))
  ENDELSE

  search = STRUPCASE(search)
  FOR i=0, N_ELEMENTS(search)-1 DO BEGIN
     j = WHERE(fields EQ search[i])
     IF (j[0] EQ -1) THEN $
        MESSAGE, 'Field ' + search[i] + ' is not present in input structure'

     IF (KEYWORD_SET(keep)) THEN $
        keep_id[j] = 1 $
     ELSE $
        keep_id[j] = 0        
  ENDFOR

  keep_id = WHERE(keep_id)
  IF (keep_id[0] EQ -1) THEN $
     MESSAGE, 'No field selected for output'
  fields = fields[keep_id]
  

  ;;Should we rename fields?
  IF (KEYWORD_SET(_rename)) THEN BEGIN
     IF (N_ELEMENTS(_rename) NE N_ELEMENTS(fields)) THEN $
        MESSAGE, 'RENAME content is not compatible with selected output'
     rename = _rename
  ENDIF $
  ELSE $
     rename = fields ;;keep orignal names

  i = WHERE(rename EQ '')
  IF (i[0] NE -1) THEN $
     rename[i] = fields[i]

  ;;Create template structure
  ll = 'templ = { ' + STRJOIN(rename + ': struct[0].' + fields, ', ') + '}'
  IF (~EXECUTE(ll)) THEN $
     MESSAGE, 'Error while creating template for output structure'

  ;;Output
  ret = REPLICATE(templ, N_ELEMENTS(struct))

  ;;Copy data
  FOR i=0, N_ELEMENTS(fields)-1 DO $
     ret.(i) = struct.(keep_id[i])

  RETURN, ret
END
