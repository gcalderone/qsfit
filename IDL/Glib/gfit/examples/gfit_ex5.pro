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

PRO gfit_ex5
  COMMON GFIT

  ;;Generate fake data 
  gfit_ex_fakedata, x, y, e

  ;;Initialize gfit
  gfit_init

  ;;Split data set (i.e. simulate a single phenomenon observed with
  ;;two different detector)
  nn = gn(x) / 2
  x1 = x[0:nn-1]
  y1 = y[0:nn-1]
  e1 = e[0:nn-1]

  x2 = x[nn:*]
  y2 = y[nn:*]
  e2 = e[nn:*]

  ;;Simulate wrong calibration among the two detectors
  y2 *= 0.8
  e2 *= 0.8
  y2 += 1.3

  ;;Initialize gfit
  gfit_init

  ;;Add data into gfit
  gfit_add_data, x1, y1, e1, label='det1'
  gfit_add_data, x2, y2, e2, label='det2'

  ;;Add components to model
  gfit_add_comp, type='gfit_comp_simplepar', 'Continuum'
  gfit_add_comp, type='gfit_comp_Gauss'    ,['line1', 'line2'] ;;add two component at once

  ;;A (very fast) way to add several component of the same kind is to
  ;;prepare the component structure usign "gfit_component"...
  comp = gfit_component('gfit_comp_simplepar')

  ;;...and add them directly
  comp.par.val   = 1
  gfit_add_comp, type=comp, 'calib_scale'

  comp.par.val   = 0
  gfit_add_comp, type=comp, 'calib_offset'

  ;;Prepare model expression (one for each detector)
  gfit.expr.det1.model = 'continuum + line1 + line2'
  gfit.expr.det2.model = 'calib_offset + calib_scale * (continuum + line1 + line2)'

  ;;Secondary expressions to be plotted (these are automatically added
  ;;for both detectors)
  gfit_add_expr, 'plot_line1', 'continuum + line1'
  gfit_add_expr, 'plot_line2', 'continuum + line2'

  ;;Guess parameters
  gfit.comp.continuum.par.val  = 2.
  gfit.comp.line1.norm.val     = 0.5
  gfit.comp.line1.center.val   = 0.1
  gfit.comp.line1.sigma.val    = 0.5
  gfit.comp.line2.norm.val     = 0.5
  gfit.comp.line2.center.val   = 0.9
  gfit.comp.line2.sigma.val    = 0.2

  ;;Run fit
  gfit_compile
  gfit_run
  gfit_report

  ;;Plot results
  gfit_plot, 0  &  ggp
  gfit_plot, 1  &  ggp
  gfit_plot_resid, 0  &  ggp
  gfit_plot_resid, 1  &  ggp


  ;;The lines plotted for DET2 do not account for calibration
  ;;coefficients.  Add two more expressions (only for DET2) to plot
  ;;correct lines.  Note that we need to recompile the model.
  gfit_add_expr, dataset='det2', 'calib_line1', 'calib_offset + calib_scale * (continuum + line1)'
  gfit_add_expr, dataset='det2', 'calib_line2', 'calib_offset + calib_scale * (continuum + line2)'
  gfit_compile
  gfit_report

  ;;Plot results
  gfit_plot, 0  &  ggp
  gfit_plot, 1  &  ggp
  gfit_plot_resid, 0  &  ggp
  gfit_plot_resid, 1  &  ggp
END
