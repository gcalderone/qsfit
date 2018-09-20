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
;  gfit_nfree
;
;PURPOSE:
;  Return how many free parameters are in the model
;
;PARAMETERS:
;
;RETURN VALUE: (array of structures)
;  Number of free parameters.
;
FUNCTION gfit_nfree
  COMPILE_OPT IDL2
  ON_ERROR, !glib.on_error
  COMMON GFIT

  par = gfit_get_par()
  IF (gn(par) EQ 0) THEN RETURN, 0
  RETURN, gsearch(par.fixed EQ 0, /count)
END
