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

PRO gfit_ex3
  COMMON GFIT

  ;;Generate fake data 
  gfit_ex_fakedata, x, y, e

  ;;Initialize gfit
  gfit_init

  ;;Add data into gfit
  gfit_add_data, x, y, e, label='Data'

  ;;Add components to model
  gfit_add_comp, type='gfit_comp_simplepar', 'Continuum'
  gfit_add_comp, type='gfit_comp_Gauss'    , 'line1'
  gfit_add_comp, type='gfit_comp_Gauss'    , 'line2'

  ;;Model expression
  gfit.expr.(0).model = 'continuum + line1 + line2'

  ;;Secondary expressions to be plotted
  gfit_add_expr, 'plot_cont' , 'continuum'
  gfit_add_expr, 'plot_line1', 'continuum + line1'
  gfit_add_expr, 'plot_line2', 'continuum + line2'

  ;;Guess parameters
  gfit.comp.continuum.par.val  = 1.
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

  ;;Setup plot options
  gfit.plot.(0).plot_cont.gp  = 'w l dt 2 lw 2 lc rgb "red"'
  gfit.plot.(0).plot_line1.gp = 'w l lc rgb "blue"'
  gfit.plot.(0).plot_line2.gp = 'w l lc rgb "dark-green"'

  ;;Plot results
  gfit_plot        &  ggp
  gfit_plot_resid  &  ggp
END
