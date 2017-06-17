; *******************************************************************
; Copyright (C) 2016-2017 Giorgio Calderone
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
;  gfit_eval
;
;PURPOSE:
;  Evaluate the model, compute the "fit" statistic, the "test"
;  statistic, and return the secondary expressions evaluation.
;
;PARAMETERS:
;  EXPR=  (output, a scalar structure)
;    The returned value contains all the evaluated expressions. The
;    structure is the same as GFIT.expr, but all field are either
;    scalar numbers or arrays of numbers, instead of strings.
;
PRO gfit_eval, expr=expr
  COMPILE_OPT IDL2
  ON_ERROR, !glib.on_error
  COMMON GFIT

  ;;This procedure may be called from user after the model has been
  ;;changed, hence we delete the MPFIT status and elapsed time.
  gfit.res.mpfit_status = 0
  gfit.res.elapsed_time = gnan()

  ;;Evaluate the model and get the weighted deviations
  wdev = mpfit_eval_model((gfit_get_par()).val, expr=expr)

  ;;The "fit" statistic is the chi-squared, i.e. the statstic
  ;;minimized by MPFIT.
  gfit.res.fit_stat = TOTAL( wdev^2. )
  
  ;;Delete current "test" statistic data
  gfit.res.test_stat = gnan()
  gfit.res.test_dof  = -1
  gfit.res.test_prob = gnan()

  IF (~gfit.opt.gof_test) THEN RETURN

  ;;Collect all CMP structures and call routine appropriate for given
  ;;data type.  The purpose of these routines is to populate the
  ;;gfit.res structure using data from input parameter.  Check
  ;;gfit_teststat_gauss for an example
  cmp = []
  FOR i=0, gfit.data.nn-1 DO $
     cmp = [cmp, gfit.cmp.(i)]

  ;;Compute "test" statistic data
  CALL_PROCEDURE, 'gfit_teststat_' + gfit.opt.data_type, cmp
END

