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
;  gfit_plot_data
;
;PURPOSE:
;
;PARAMETERS:
;  IDATA (optional input, a scalar integer)
;    The index of the data set to plot.  If not given the first
;    dataset (i.e. IDATA=0) is assumed.
;
FUNCTION gfit_plot_data, idata
  COMPILE_OPT IDL2
  ON_ERROR, !glib.on_error
  COMMON GFIT

  ;;Data available?
  IF (gfit.data.nn EQ 0) THEN RETURN, []

  ;;Evaluate model
  gfit_eval, expr=expr

  IF (N_PARAMS() EQ 0) THEN idata = 0
  out = gfit.cmp.(idata)


  ;;Add data for secondary expressions
  FOR j=1, N_TAGS(expr.(idata))-1 DO BEGIN ;start from 1 since the first one is the model
     out = gstru_insert(out, (TAG_NAMES(expr.(idata)))[j], 0.)
     out.(N_TAGS(out)-1) = expr.(idata).(j)
  ENDFOR

  RETURN, out
END




