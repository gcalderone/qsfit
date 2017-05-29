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
;GFIT MODEL COMPONENT
;
;NAME:
;  gfit_comp_gauss
;
;COMPONENT DESCRIPTION:
;  A Gaussian profile in the form:
;    A * EXP(-((X - CENTER) / SIGMA)^2 / 2)
;
;  where A is either:
;    NORM                              (if MAX=1)
;    NORM / (SQRT(2 * !PI) * SIGMA)    (if MAX=0)
;
;PARAMETERS:
;  NORM (if MAX=1 units are [Y], if MAX=0 units are [X*Y])
;    Component normalization (see the MAX option).
;
;  CENTER (units: [X])
;    Center value of the gaussian profile.
;
;  SIGMA (units: [X])
;    Width of the Gaussian profile.  The parameter must always be
;    limited to have a positive value.
;
;OPTIONS:
;  MAX: (either 0 or 1)
;    If MAX=1 the NORM parameter is the component value at X=CENTER.
;    If MAX=0 THe NORM parameter is the integral of the Gaussian
;    profile.
;

;=====================================================================
PRO gfit_comp_Gauss_init, comp
  comp.norm.val  = 1
  comp.sigma.val = 1
  comp.sigma.limits = [1.e-4, gnan()]  ;;Lower limit should always be positive.
END


;=====================================================================
FUNCTION gfit_comp_Gauss, x, norm, center, sigma, MAX=max
  COMPILE_OPT IDL2
  ON_ERROR, !glib.on_error
 
  exp = ((x-center) / sigma)^2 / 2. < 80
  ret = norm * EXP(-exp)

  ;;Norm parameter is the integral, unless the MAX keyword is set
  IF (~KEYWORD_SET(max)) THEN $ 
     ret /= SQRT(2*!PI) * sigma

  RETURN, ret
END

