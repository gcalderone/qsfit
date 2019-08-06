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
;  qsfit_comp_emline
;
;COMPONENT DESCRIPTION:
;  An emission line with Gaussian profile in the form:
;    NORM * EXP(-((X - x0) / Sigma)^2 / 2) / (SQRT(2 * !PI) * Sigma)
;
;  where x0 and Sigma are the center wavelength and the width of the
;  emission line profile, whose values depends on the CENTER, V_OFF
;  and FWHM parameters.
;
;PARAMETERS:
;  NORM (units: [X*Y])
;    Total flux in the emission line.
;
;  CENTER (units: [X])
;    Rest frame wavelength of the emission line.
;
;  V_OFF (units: km s^-1)
;    Velocity offset of the emission line with respect to the CENTER
;    wavelength.  Positive value means the line is shifted towards
;    smaller wavelengths.
;
;  FWHM (units: km s^-1)
;    Full-width at half maximum of the emission line.
;
;OPTIONS:
;  NONE
;
;NOTES:
;  Only one parameter among CENTER and V_OFF should be free to vary,
;  the other one must be freezed.
;

PRO qsfit_comp_emline_init, comp
  comp.par.norm.limits[0] = 0

  comp.par.center.fixed = 1

  comp.par.v_off.val = 0
  comp.par.v_off.step  = 1

  comp.par.fwhm.limits[0] = 0
END


FUNCTION qsfit_comp_emline, x, norm, center, v_off, fwhm
  COMPILE_OPT IDL2
  ON_ERROR, !glib.on_error

  IF (norm EQ 0) THEN RETURN, 0.

  IF (!QSFIT_OPT.lorentzian) THEN BEGIN
     x0 = center - (v_off / 3.e5) * center
     w = fwhm / 3.e5 * center
     xx = (x - x0) / (w / 2.)
     RETURN, FLOAT(norm / (1 + xx^2.) / (w * !PI/2))
  ENDIF

  x0    = center - (v_off / 3.e5) * center
  sigma =          (fwhm  / 3.e5) * center / 2.35

  ;exp = ((x-x0) / sigma)^2. / 2.
  ;i = WHERE(exp LT 10) ;;improve performance
  ;line = norm * EXP( -exp[i] ) / 2.50663 / sigma ;SQRT(2*!PI) = 2.50663

  ;;improve performance (SQRT(10*2.) = 4.4721360)
  i = WHERE(ABS(x-x0) LT 4.4721360 * sigma)
  exp = ((x[i]-x0) / sigma)^2. / 2.
  line = norm * EXP( -exp ) / 2.50663 / sigma ;SQRT(2*!PI) = 2.50663

  retval = FLTARR(N_ELEMENTS(x))
  retval[i] = line
  RETURN, retval
END
