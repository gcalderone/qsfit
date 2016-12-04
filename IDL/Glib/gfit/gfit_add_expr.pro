; *******************************************************************
; Copyright (C) 2016 Giorgio Calderone
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
;  gfit_add_expr
;
;PURPOSE:
;  Add secondary expressions to current GFIT structure.
;
;DESCRIPTION:
;  Secondary expressions are only evaluated on request (through the
;  gfit_eval, EXPR= keyword), they are not evaluated during the
;  fitting process to save computation time.  Secondary expressions
;  are also plotted using gfit_plot, and the plotting options can be
;  changed using gfit.plot.DATA_SET_NAME.EXPRESSION_NAME. Secondary
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
PRO gfit_add_expr, label, expr, DATASET=dataset
  COMPILE_OPT IDL2
  ON_ERROR, !glib.on_error
  COMMON GFIT_PRIVATE
  COMMON GFIT

  IF ((gn(label) NE 1)   OR   $
      (gn(expr)  NE 1))  THEN $
     MESSAGE, 'LABEL and EXPR must be scalar strings'

  IF (~KEYWORD_SET(dataset)) THEN $
     dataset = (TAG_NAMES(gfit.data))[0:gfit.data.nn-1]

  IF (gn(dataset) GT 1) THEN BEGIN
     FOR i=0, gn(dataset)-1 DO $
        gfit_add_expr, label, expr, dataset=dataset[i]
     RETURN
  ENDIF

  IF (gtype(dataset) EQ 'STRING') THEN BEGIN
     IF (~gsearch(TAG_NAMES(gfit.data) EQ STRUPCASE(dataset), idata)) THEN $
        MESSAGE, 'Data set do not exists: ' + dataset
  ENDIF $
  ELSE BEGIN
     idata = dataset
     IF (idata GE gfit.data.nn) THEN $
        MESSAGE, 'Data set does not exists: ' + gn2s(idata)
  ENDELSE

  IF (gsearch(TAG_NAMES(gfit.comp) EQ STRUPCASE(label))) THEN $
     MESSAGE, 'A component named ' + label + ' already exists.'

  e = gfit.expr.(idata)
  IF (gsearch(TAG_NAMES(e) EQ STRUPCASE(label))) THEN $
     MESSAGE, 'An expression named ' + label + ' already exists.'

  e = CREATE_STRUCT(e, label, expr)

  plotopt = template_expr_plotopt
  plotopt.label = label
  p = gfit.expr.(idata)
  p = CREATE_STRUCT(gfit.plot.(idata), label, plotopt)

  sexpr = []
  splot = []
  FOR i=0, gfit.data.nn-1 DO BEGIN
     IF (i EQ idata) THEN BEGIN
        sexpr = CREATE_STRUCT(sexpr, (TAG_NAMES(gfit.expr))[i], e)
        splot = CREATE_STRUCT(splot, (TAG_NAMES(gfit.plot))[i], p)
     ENDIF $
     ELSE  BEGIN
        sexpr = CREATE_STRUCT(sexpr, (TAG_NAMES(gfit.expr))[i], gfit.expr.(i))
        splot = CREATE_STRUCT(splot, (TAG_NAMES(gfit.plot))[i], gfit.plot.(i))
     ENDELSE
  ENDFOR

  gfit = { $
           opt:   gfit.opt     $
         , data:  gfit.data    $
         , comp:  gfit.comp    $
         , expr:  sexpr        $
         , cmp:   gfit.cmp     $
         , res:   template_res $
         , plot:  splot        $
         }
END
