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
;  qsfit_comp_sbpowerlaw
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
  comp.par.curv.val = 1             ;lowest curvature
  comp.par.curv.limits = [1, 1000]
END


FUNCTION qsfit_comp_sbpowerlaw_cdata, comp, x, cdata
  IF (gn(cdata) GT 0) THEN RETURN, cdata

  ;;Compute LOG(X) the first time the function is called.
  xx = DOUBLE(gloggen(MIN(x), MAX(x), 100))
  cdata = { $
          xx: xx            , $
          logx: ALOG(xx)    , $
          res: REPLICATE(DOUBLE(gnan()), gn(xx)), $
          eval: REPLICATE(gnan(), 2) $
          }
  RETURN, PTR_NEW(cdata)
END


FUNCTION qsfit_comp_sbpowerlaw, x, norm, x0, alpha1, dalpha, curv, cdata=cdata
  COMPILE_OPT IDL2
  ON_ERROR, !glib.on_error

  s = 1.
  IF (dalpha LT 0) THEN s = -1.
  da = ABS(dalpha) ;;ABS(alpha2 - alpha1)

  ;;Use logarithms to avoid overflows and improve performance
  ret = EXP(                         $
        alpha1 * ((*cdata).logx - ALOG(x0)) + $
        s/curv * (  ALOG(1.d + ((*cdata).xx/x0)^(da*curv)) - ALOG(2.d)  ) $
           )
  ;;IF (CHECK_MATH(mask=208,/NOCLEAR) NE 0) THEN STOP

  (*cdata).res = ret * norm
  (*cdata).eval = INTERPOL((*cdata).res, (*cdata).xx, [2350., 3000.])

  RETURN, FLOAT(INTERPOL(ret * norm, (*cdata).xx, x))
END

FUNCTION qsfit_comp_sbpowerlaw_l2350
  COMMON GFIT
  cdata = gfit_cdata.i0_continuum
  RETURN, (*cdata).eval[0]
END

FUNCTION qsfit_comp_sbpowerlaw_l3000
  COMMON GFIT
  cdata = gfit_cdata.i0_continuum
  RETURN, (*cdata).eval[1]
END
