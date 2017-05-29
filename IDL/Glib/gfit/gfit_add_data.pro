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
;NAME:
;  gfit_add_data
;
;PURPOSE:
;  Add a new data set into GFIT.
;
;DESCRIPTION:
;  Each data set has an independent quantity (X), a dependent quantity
;  (Y) and an associated uncertainty (E).  The GFIT model will be
;  evaluated on all the provided X values, and the model results will
;  be compared with the provided Y values. Data sets are stored in
;  GFIT.data.
;
;PARAMETERS:
;  X, Y, E (input, an array of numbers)
;    The data set independent quantity, dependent quantity or measure,
;    and the associated uncertainty.
;
;  UDATA= (optional input, any type)
;    A variable which will be stored in the GFIT structure for user
;    convenience.
;
;  LABEL= (optional input, a scalar string)
;    The name of the data set.  This will be the name of a field in
;    the gfit.data structure, hence it ust be a valid IDL name.  Also,
;    it should not clash with other data set names.
;
;NOTES:
;  The model must be recompiled (using gfit_compile) after a call to
;  to this procedure.
;
PRO gfit_add_data, x, y, e, UDATA=udata, LABEL=label
  COMPILE_OPT IDL2
  ON_ERROR, !glib.on_error
  COMMON GFIT_PRIVATE
  COMMON GFIT

  IF (~KEYWORD_SET(label)) THEN label = 'd' + gn2s(gfit.data.nn)

  IF (STRUPCASE(gfit.opt.data_type) EQ 'POISSON') THEN BEGIN
     IF (gn(e) NE 0) THEN $
        gprint, /warn, 'Errors are ignored in POISSON mode!'
     e = y*gnan()
  ENDIF

  IF (gn(x) NE gn(y)) THEN MESSAGE, 'X and Y input arrays must have the same length'
  IF (gn(e) NE gn(y)) THEN MESSAGE, 'Y and ERR input arrays must have the same length'
  IF (~KEYWORD_SET(udata)) THEN udata = 0b

  ;;Prepare the data set structure
  data = {  label: label,  $
            x: FLOAT(x) ,  $
            Y: FLOAT(y) ,  $
            E: FLOAT(e),   $
            group: LONARR(gn(x)), $
            udata: udata }

  ;;By default consider all available data
  data.group = 1


  expr = CREATE_STRUCT('model', '')

  plot = CREATE_STRUCT('main' , template_main_plotopt, $
                       'data' , template_expr_plotopt, $
                       'model', template_expr_plotopt)

  plot.main.title = label
  plot.data.label = label
  plot.data.gp    = 'with yerrorbars pt 0 lt rgb "black"'

  plot.model.label = 'Model'
  plot.model.gp    = 'with line ls 1 dt 1 lw 2 lt rgb "orange"'

  ;;Append to current structures
  data = gstru_insert(gfit.data, label, data, gfit.data.nn)
  expr = gstru_insert(gfit.expr, label, expr, gfit.data.nn)
  plot = gstru_insert(gfit.plot, label, plot, gfit.data.nn)
  data.nn += 1

  gfit = { $
           opt:   gfit.opt   $
         , data:  data       $
         , comp:  gfit.comp  $
         , expr:  expr       $
         , cmp:   gfit.cmp   $
         , res:   template_res $
         , plot:  plot       $
         }
END
