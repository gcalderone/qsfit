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
;  ggp_get_state
;
;PURPOSE:
;  Return a structure containing the whole current state.  Such state
;  can be saved to a file, and then restored using ggp_set_state.
;
;PARAMETERS:
;  NONE
;
;RETURN VALUE:
;  A structure containing current state.
;
FUNCTION ggp_get_state
  COMPILE_OPT IDL2
  ON_ERROR, !glib.on_error
  COMMON COM_GGP

  ret = []
  IF (gn(ggp_cmd ) GT 0) THEN BEGIN
     ret = CREATE_STRUCT(ret, '_cmd'   , ggp_cmd.toarray()  )
     ret = CREATE_STRUCT(ret, '_cmd_m' , ggp_cmd_m.toarray()) 
  ENDIF $
  ELSE BEGIN
     ret = CREATE_STRUCT(ret, '_cmd'   , '')
     ret = CREATE_STRUCT(ret, '_cmd_m' , '')
  ENDELSE

  IF (gn(ggp_plot) GT 0) THEN BEGIN
     ret = CREATE_STRUCT(ret, '_plot'  , ggp_plot.toarray()  )
     ret = CREATE_STRUCT(ret, '_plot_m', ggp_plot_m.toarray())
  ENDIF $
  ELSE BEGIN
     ret = CREATE_STRUCT(ret, '_plot'  , '' )
     ret = CREATE_STRUCT(ret, '_plot_m', '' )
  ENDELSE

  IF (gn(ggp_data) GT 0) THEN BEGIN
     ret = CREATE_STRUCT(ret, ggp_data)
  ENDIF

  RETURN, ret
END
