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

PRO gfit_ex1
  COMMON GFIT

  ;;Generate fake data 
  gfit_ex_fakedata, x, y, e

  ;;Initilize GFIT
  gfit_init

  ;;Add data into gfit
  gfit_add_data, x, y, e

  ;;Add components to model.  Default component type is gfit_comp_par
  gfit_add_comp, 'Continuum'
  gfit_add_comp, 'norm1'
  gfit_add_comp, 'center1'
  gfit_add_comp, 'sigma1'
  gfit_add_comp, 'norm2'
  gfit_add_comp, 'center2'
  gfit_add_comp, 'sigma2'

  ;;Model expression
  gfit.expr.(0).model = 'continuum + norm1 * EXP(-(x - center1)^2 / (2 * sigma1^2)) / SQRT(2*!PI) / sigma1 + norm2 * EXP(-(x - center2)^2 / (2 * sigma2^2)) / SQRT(2*!PI) / sigma2'

  ;;Secondary expressions to be plotted
  gfit_add_expr, 'line1', 'norm1 * EXP(-(x - center1)^2 / (2 * sigma1^2)) / SQRT(2*!PI) / sigma1'
  gfit_add_expr, 'line2', 'norm2 * EXP(-(x - center2)^2 / (2 * sigma2^2)) / SQRT(2*!PI) / sigma2'

  ;;Guess parameters
  gfit.comp.continuum.par.val  = 1. 
  gfit.comp.norm1.par.val      = 0.5
  gfit.comp.center1.par.val    = 0.1
  gfit.comp.sigma1.par.val     = 0.5
  gfit.comp.norm2.par.val      = 0.5
  gfit.comp.center2.par.val    = 0.9
  gfit.comp.sigma2.par.val     = 0.2

  ;;Run fit
  gfit_compile
  gfit_run
  gfit_report

  ;;Plot results
  gfit_plot        &  ggp
  gfit_plot_resid  &  ggp
END
