; *******************************************************************
; Copyright (C) 2016-2018 Giorgio Calderone
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
;  gfit_restore
;
;PURPOSE:
;  Restore a GFIT session.
;
;PARAMETERS:
;  SAVED (input, a GFIT structure)
;    A GFIT structure previously copied from GFIT common block.
;
PRO gfit_restore, saved
  COMPILE_OPT IDL2
  ON_ERROR, !glib.on_error
  COMMON GFIT

  ;;Collect component names
  comp = STRARR(N_TAGS(saved.comp))
  FOR i=0, N_TAGS(saved.comp)-1 DO  comp[i] = saved.comp.(i).funcName
  comp = comp[SORT(comp)]
  comp = comp[UNIQ(comp)]
  
  ;;Initialize components
  FOR i=0, gn(comp)-1 DO dummy = gfit_component(comp[i])

  ;;Copy structure and compile
  gfit = saved
  gfit_compile
  gfit = saved ;;this is needed since gfit_compile drops the RES structure
END
