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
;  qsfit_comp_emline
;
;COMPONENT DESCRIPTION:
;  A smoothly broken power law in the form:
;    NORM *
;      (X / X0)^ALPHA1     *
;      ((1 + (X / X0)^(ABS(DALPHA) * CURV)) / 2)^(S / CURV)
;
;  where NORM is the component value at X=X0, X0 is the break
;  wavelength, ALPHA1 is the spectral index at wavelength much smaller
;  than X0, ALPHA+DALPHA is the spectral index at wavelength much
;  larger than X0, CURV is the "curvature" parameter, and S if s
;  either +1 or -1 according to the sign of DALPHA.
;
;PARAMETERS:
;  NORM (units: [Y])
;    Component value at X=X0.
;
;  X0 (units: [X])
;    Break wavelength.
;
;  ALPHA1 (no units)
;    Spectral index at X<<X0.
;
;  DALPHA (no units)
;    Change in spectral index.  At X>>X0 the spectral index is
;    ALPHA1+DALPHA.
;
;  CURV (no units)
;    "Curvature" parameter, sets how abrupt is the change in slope.
;    The component will behave like a power law with slope ALPHA1 at
;    wavelengths smaller than X1 (< X0), while it will have a slope
;    ALPHA2 at wavelengths larger than X2 (> X0).  The ratio of the X2
;    and X1 wavelengths is approximately given by:
;
;      log_10(X2 / X1) ~ 2 / (ABS(DALPHA) * CURV)
;
;    The CURV value must be greater than 1.
;
;OPTIONS:
;  NONE
;
PRO qsfit_comp_sbpowerlaw_init, comp
  COMMON COM_QSFIT_COMP_SBPOWERLAW, $
     xx, logx, log2, last_norm, last_x0, last_a1, last_da, last_curv, last_res, last_l3000
  comp.curv.val = 1             ;lowest curvature
  comp.curv.limits = [1, 1000]
  xx = []
  last_norm = gnan()
  last_x0   = gnan()
  last_a1   = gnan()
  last_da   = gnan()
  last_curv = gnan()
  last_res  = gnan()
  last_l3000= 1.
END


FUNCTION qsfit_comp_sbpowerlaw, x, norm, x0, alpha1, dalpha, curv
  COMPILE_OPT IDL2
  ON_ERROR, !glib.on_error
  COMMON COM_QSFIT_COMP_SBPOWERLAW

  ;;Compute LOG(X) the first time the function is called.
  IF (gn(xx) EQ 0) THEN BEGIN
     xx = DOUBLE(gloggen(MIN(x), MAX(x), 100))
     logx = ALOG(xx)
     log2 = ALOG(2.d)
  ENDIF

  IF (last_norm EQ norm   AND   $
      last_x0   EQ x0     AND   $
      last_a1   EQ alpha1 AND   $
      last_da   EQ dalpha AND   $
      last_curv EQ curv  ) THEN $
         RETURN, last_res

  last_norm = norm  
  last_x0   = x0    
  last_a1   = alpha1
  last_da   = dalpha
  last_curv = curv  


  s = 1.
  IF (dalpha LT 0) THEN s = -1.
  da = ABS(dalpha) ;;ABS(alpha2 - alpha1)
  
  ;;Use logarithms to avoid overflows and improve performance
  ret = EXP(                         $
        alpha1 * (logx - ALOG(x0)) + $
        s/curv * (  ALOG(1.d + (xx/x0)^(da*curv)) - log2  ) $
           )
  ;;IF (CHECK_MATH(mask=208,/NOCLEAR) NE 0) THEN STOP  
  last_res = FLOAT(INTERPOL(ret * norm, xx, x))
  last_l3000 = FLOAT(INTERPOL(ret * norm, xx, 3000.))
  
  ;;last_res = FLOAT(INTERPOL(norm * EXP(alpha1 * (logx - ALOG(x0))), xx, x))
  ;;last_res = FLOAT(INTERPOL(norm * (xx/x0)^alpha1, xx, x))

  RETURN, last_res
END

