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
;  LABEL= (input, a scalar string)
;    The name of the data set.  This will be the name of a field in
;    the gfit.data structure, hence it ust be a valid IDL name.  Also,
;    it should not clash with other data set names.
;
;  X, Y, E (input, an array of numbers)
;    The data set independent quantity, dependent quantity or measure,
;    and the associated uncertainty.
;
;  UDATA= (optional input, any type)
;    A variable which will be stored in the GFIT structure for user
;    convenience.
;
;NOTES:
;  The model must be recompiled (using gfit_compile) after a call to
;  to this procedure.
;
PRO gfit_add_data, x, y, e, UDATA=udata, GROUP=group, IOBS=iobs
  COMPILE_OPT IDL2
  ON_ERROR, !glib.on_error
  COMMON GFIT

  IF (gn(x) NE gn(y)) THEN MESSAGE, 'X and Y input arrays must have the same length'
  IF (gn(e) NE gn(y)) THEN MESSAGE, 'Y and ERR input arrays must have the same length'
  IF (gn(group) NE gn(x)) THEN group = REPLICATE(1, gn(x))   ;;consider all available data
  IF (~KEYWORD_SET(udata)) THEN udata = 0b

  IF (N_TAGS(gfit.obs) EQ 0) THEN gfit_add_obs
  IF (gn(iobs) EQ 0) THEN iobs = N_TAGS(gfit.obs)-1
  IF (iobs LT 0  OR  $
      iobs GE N_TAGS(gfit.obs)) THEN $
     MESSAGE, 'IOBS=' + gn2s(iobs) + ' is not a valid ID'

  ;;Prepare the data set structure
  obs = gfit.obs.(iobs)
  id = N_TAGS(obs.data)
  data = {  x: FLOAT(x) ,  $
            y: FLOAT(y) ,  $
            e: FLOAT(e) ,  $
            group: group,  $
            udata: udata,  $
            plot: { enable:   1b    , $                               ;;Enable (1) or disable (0) the plot of this expression.
                    label:    'Data' + gn2s(id), $                    ;;Label shown in plot legend.
                    gp:       'with yerrorbars pt 0 lt rgb "black"' $ ;;Gnuplot format
                  } $
         }

  tmp = (id EQ 0  ?  {i0: data}  :  CREATE_STRUCT(obs.data, 'i'+gn2s(id), data))
  obs = {expr: obs.expr, aux: obs.aux, data: tmp, eval: 0, plot: obs.plot}

  ;; Replace obs object.
  gfit_replace_obs, iobs, obs
END
