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
;  gfit_set_parval
;
;PURPOSE:
;  Save the parameter values into the GFIT structure.
;
;PARAMETERS:
;  PVAL  (input, array of floating point numbers)
;    New values to be stored in the GFIT structure.  There must be a
;    value for each parameter in the model.
;
;  PERR  (input, array of floating point numbers)
;    New uncertainties values to be stored in the GFIT structure.
;    There must be a value for each parameter in the model.
;
PRO gfit_set_parval, pval, perr
  COMPILE_OPT IDL2
  ON_ERROR, !glib.on_error
  COMMON GFIT

  count = 0
  IF (N_TAGS(gfit.comp) EQ 0) THEN RETURN
  FOR i=0, N_TAGS(gfit.comp)-1 DO BEGIN
     IF (~gfit.comp.(i).enabled) THEN CONTINUE
     FOR j=0, gfit.comp.(i).npar-1 DO BEGIN
        par = gfit.comp.(i).par.(j)
        par.val = pval[count]
        par.err = perr[count]

        ;;Check param value is within the limit
        IF ((par.val EQ par.limits[0])   OR   $
            (par.val EQ par.limits[1]))  THEN BEGIN
           par.err  = gnan()
        ENDIF
        gfit.comp.(i).par.(j) = par
        count += 1
     ENDFOR
  ENDFOR

  IF (gn(pval) NE count) THEN STOP
END
