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
;  gfit_get_covar
;
;PURPOSE:
;  Returns the parameter covariance matrix in a tabular form.
;
;PARAMETERS:
;  NONE.
;
;RETURN VALUE: (an array of structures)
;  Each element in the returned array refers to a couple of model
;  parameters.  The components/parameter name of the first parameter
;  is given in fields COMP1,PAR1, while those of the second parameter
;  are given in COMP2,PAR2.  The covariance value is given in the
;  COVAR field.
;
FUNCTION gfit_get_covar
  COMPILE_OPT IDL2
  ON_ERROR, !glib.on_error
  COMMON GFIT

  IF (gn(gfit.res.covar) EQ 1) THEN $
     MESSAGE, 'The covariance matrix is not available.  Call gfit_run to fit the data and evaluate the matrix.'

  ;;Renormalize covariance matrix
  par   = gfit_get_par()
  perr  = par.err # par.err
  inan  = WHERE(~FINITE(perr))
  izero = WHERE(perr EQ 0)
  perr[inan ] = 1.
  perr[izero] = 1.
  covar = gfit.res.covar / perr
  covar[inan ] = gnan()
  covar[izero] = gnan()


  ret = REPLICATE({comp1: '', par1: '', comp2: '', par2: '', covar: gnan()}, gn(covar))
  FOR i=0, gn(covar)-1 DO BEGIN
     j = ARRAY_INDICES(covar, i)
     IF (j[0] EQ j[1]) THEN CONTINUE
     ret[i].comp1 = par[j[0]].comp
     ret[i].par1  = par[j[0]].parname
     ret[i].comp2 = par[j[1]].comp
     ret[i].par2  = par[j[1]].parname
     ret[i].covar = covar[i]
     covar[j[0], j[1]] = gnan()
     covar[j[1], j[0]] = gnan()
  ENDFOR
  ret = ret[WHERE(FINITE(ret.covar))]
  ret = ret[REVERSE(SORT(ABS(ret.covar)))]

  RETURN, ret
END

