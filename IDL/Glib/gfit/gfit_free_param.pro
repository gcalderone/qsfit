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
;  gfit_free_param
;
;PURPOSE:
;  Return the number of free parameters in the model.
;
;PARAMETERS:
;  NONE.
;
;RETURN VALUE:  (a scalar integer number)
;  The number of free parameters in the model, i.e. how many
;  parameters are not fixed or tied. 
;  
FUNCTION gfit_free_param
  COMPILE_OPT IDL2
  ON_ERROR, !glib.on_error

  par = gfit_get_par()
  dummy = gsearch((par.fixed EQ 0)   AND   (par.tied EQ ''), count=count)
  RETURN, count
END
