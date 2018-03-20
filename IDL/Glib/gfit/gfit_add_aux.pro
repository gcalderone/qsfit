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
;  gfit_add_aux
;
;PURPOSE:
;  Add auxiliary expressions to current GFIT structure.
;
;DESCRIPTION:
;  Auxiliary expressions are only evaluated on request (through the
;  gfit_eval, AUX= keyword), they are not evaluated during the fitting
;  process to save computation time.  Auxiliary expressions are also
;  plotted using gfit_plot, and the plotting options can be changed
;  using gfit.plot.DATA_SET_NAME.EXPRESSION_NAME. Auxiliary
;  expressions are stored in GFIT.expr.
;
;PARAMETERS:
;  LABEL (input, a scalar string)
;    The name of the expression.  This will be the name of a field in
;    the gfit.expr.DATA_SET_NAME structure, hence it ust be a valid
;    IDL name.  Also, it should not clash with other expression or
;    component names.
;
;  EXPR (input, a scalar string)
;    The IDL mathematical expression to be evaluated.  The expression
;    can refers to the components using their name.
;
;  DATASET= (optional input, a scalar or array of strings)
;    The list of data set names for which the expression should be
;    evaluated.  If this input is not given the expression is added
;    for all currently defined data sets.
;
;NOTES:
;  The model must be recompiled (using gfit_compile) after a call to
;  to this procedure.
;
PRO gfit_add_aux, label, expr, IOBS=iobs
  COMPILE_OPT IDL2
  ON_ERROR, !glib.on_error
  COMMON GFIT

  IF (N_TAGS(gfit.obs) EQ 0) THEN gfit_add_obs
  IF (gn(iobs) EQ 0) THEN iobs = N_TAGS(gfit.obs)-1
  IF (iobs LT 0  OR  $
      iobs GE N_TAGS(gfit.obs)) THEN $
     MESSAGE, 'IOBS=' + gn2s(iobs) + ' is not a valid ID'

  aux = {expr: expr, $
         plot: { enable:   1b   , $           ;;Enable (1) or disable (0) the plot of this expression.
                 label:    label, $           ;;Label shown in plot legend.
                 gp:       'with lines' $     ;;Gnuplot format
               } $
        }

  obs = gfit.obs.(iobs)
  tmp = (N_TAGS(obs.aux) EQ 0  ?  CREATE_STRUCT(label, aux)  :  CREATE_STRUCT(obs.aux, label, aux))
  obs = {expr: obs.expr, aux: tmp, data: obs.data, eval: obs.eval, plot: obs.plot}

  ;; Replace obs object.
  gfit_replace_obs, iobs, obs
END
