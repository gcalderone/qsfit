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
;GFIT MODEL COMPONENT
;
;NAME:
;  qsfit_comp_contabsorption
;
;COMPONENT DESCRIPTION:
;
;PARAMETERS:
;  slope (no units)
;
;OPTIONS:
;  NONE
;
PRO qsfit_comp_contabsorption_init, comp
  comp.par.norm.val = 0.5
  comp.par.norm.limits = [0.2, 1]
  comp.par.slope.val = 0
  comp.par.slope.limits = [0, 20]
END

FUNCTION qsfit_comp_contabsorption, x, norm, slope
  COMPILE_OPT IDL2
  ON_ERROR, !glib.on_error

  ;tmp = (1215.24 - x) > 0
  ;out = (1. - slope * tmp) > 0

  out = REPLICATE(1., gn(x))
  tmp = x / 1215.24
  i = WHERE(tmp LT 1)
  IF (i[0] EQ -1) THEN RETURN, out

  out[i] = norm * tmp[i]^(slope)
  stop

  IF (gsearch(out LE 0  OR  out GT 1)) THEN STOP
  RETURN, FLOAT(out)
END
