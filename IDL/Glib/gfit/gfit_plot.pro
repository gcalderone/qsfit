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
;  gfit_plot
;
;PURPOSE:
;  Prepare the plots of GFIT data, model and secondary expressions
;  using the GGP facility.  A further call to "ggp" is required to
;  actually do the plot.
;
;PARAMETERS:
;  IDATA (optional input, a scalar integer)
;    The index of the data set to plot.  If not given the first
;    dataset (i.e. IDATA=0) is assumed.
;
PRO gfit_plot, idata, gp=gp_file, pdf=pdf_file
  COMPILE_OPT IDL2
  ON_ERROR, !glib.on_error
  COMMON GFIT

  ;;Data available?
  IF (gfit.data.nn EQ 0) THEN RETURN

  ;;Evaluate model
  gfit_eval, expr=expr

  IF (N_PARAMS() EQ 0) THEN idata = 0
  cmp = gfit.cmp.(idata)

  ;;Special case for Poisson mode
  IF (STRUPCASE(gfit.opt.data_type) EQ 'POISSON') THEN BEGIN
     cmp.e = SQRT(cmp.y) > 1    ;ensure uncertainties are positive

     ;;Renormalize all values by the number of joined bins
     cmp.y /= cmp.nx
     cmp.e /= cmp.nx
     cmp.m /= cmp.nx
     
     FOR j=1, N_TAGS(expr.(idata))-1 DO $
        expr.(idata).(j) /= cmp.nx
  ENDIF


  ;;MAIN PLOT =====
  ggp_clear
  ggp_cmd, 'set bars 0'
  ggp_cmd, 'set clip two'
  ggp_cmd, 'set grid'
  ggp_cmd, 'set  title "' + gfit.plot.(idata).main.title + '"'
  ggp_cmd, 'set xlabel "' + gfit.plot.(idata).main.xtit  + '"'
  ggp_cmd, 'set ylabel "' + gfit.plot.(idata).main.ytit  + '"'
  ggp_cmd, 'set xrange [' + gn2s(MIN(cmp.x)) + ':' + gn2s(MAX(cmp.x)) + ']'
     
  IF (gfit.plot.(idata).main.xlog) THEN ggp_cmd, 'set logscale x'
  IF (gfit.plot.(idata).main.ylog) THEN ggp_cmd, 'set logscale y'

  x = cmp.x
  y = cmp.y
  e = cmp.e
  gfit_rebin, gfit.plot.(idata).main.rebin, x, y, e
  ggp_data, name='data', x, y, e
  ggp_plot, '$data  title "' + gfit.plot.(idata).data.label + '" ' + $
            gfit.plot.(idata).data.gp

  x = cmp.x
  y = cmp.m
  gfit_rebin, gfit.plot.(idata).main.rebin, x, y
  ggp_data, name='model', x, y
  IF (gfit.plot.(idata).model.gp EQ "") THEN $
     gfit.plot.(idata).model.gp = "with line"
  ggp_plot, '$model title "' + gfit.plot.(idata).model.label + '" ' + $
            gfit.plot.(idata).model.gp

  ;;Plot secondary expressions
  FOR j=1, N_TAGS(expr.(idata))-1 DO BEGIN ;start from 1 since the first one is the model
     x = cmp.x
     y = expr.(idata).(j)
     IF (gn(y) EQ 1) THEN y = REPLICATE(y, gn(x))
     
     opt = gfit.plot.(idata).(j+2)
     IF (~opt.plot) THEN CONTINUE
     
     gfit_rebin, gfit.plot.(idata).main.rebin, x, y
     ggp_data, getname=tmp, x, y
     label = gfit.plot.(idata).(j+2).label
     label = STRJOIN(STRSPLIT(label, '_', /extract), '\\_') ;;Escape underscore
     IF (gfit.plot.(idata).(j+2).gp EQ "") THEN $
        gfit.plot.(idata).(j+2).gp = "with line"
     ggp_plot, tmp + ' title "' + label + '" ' + gfit.plot.(idata).(j+2).gp
  ENDFOR
END




