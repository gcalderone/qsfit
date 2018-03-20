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
  gfit_add_data, x1, y1, e1
  gfit_add_obs
  gfit_add_data, x2, y2, e2

  ;;Add components to model
  gfit_add_comp, 'Continuum', 'gfit_comp_simplepar'
  gfit_add_comp, 'line1', 'gfit_comp_Gauss'
  gfit_add_comp, 'line2', 'gfit_comp_Gauss'

  ;;A (very fast) way to add several component of the same kind is to
  ;;prepare the component structure using "gfit_component"...
  comp = gfit_component('gfit_comp_simplepar')

  ;;...and add them directly
  comp.par.par.val   = 1
  gfit_add_comp, 'calib_scale', comp

  comp.par.par.val   = 0
  gfit_add_comp, 'calib_offset', comp

  ;;Prepare model expression (one for each detector)
  gfit.obs.(0).expr = 'continuum + line1 + line2'
  gfit.obs.(1).expr = 'calib_offset + calib_scale * (continuum + line1 + line2)'

  ;;Secondary expressions to be plotted (these are automatically added
  ;;for both detectors)
  gfit_add_aux, 'plot_line1', 'continuum + line1'
  gfit_add_aux, 'plot_line2', 'continuum + line2'

  ;;Guess parameters
  gfit.comp.continuum.par.par.val  = 2.
  gfit.comp.line1.par.norm.val     = 0.5
  gfit.comp.line1.par.center.val   = 0.1
  gfit.comp.line1.par.sigma.val    = 0.5
  gfit.comp.line2.par.norm.val     = 0.5
  gfit.comp.line2.par.center.val   = 0.9
  gfit.comp.line2.par.sigma.val    = 0.2

  ;;Run fit
  gfit_compile
  gfit_run
  gfit_report

  ;;Plot results
  gfit_plot, obs=0  &  ggp
  gfit_plot, obs=1  &  ggp
  gfit_plot_resid, obs=0  &  ggp
  gfit_plot_resid, obs=1  &  ggp
END
