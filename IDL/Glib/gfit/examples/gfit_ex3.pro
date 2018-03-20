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

PRO gfit_ex3
  COMMON GFIT

  ;;Generate fake data 
  gfit_ex_fakedata, x, y, e

  ;;Initialize gfit
  gfit_init

  ;;Add data into gfit
  gfit_add_data, x, y, e

  ;;Add components to model
  gfit_add_comp, 'Continuum', 'gfit_comp_simplepar'
  gfit_add_comp, 'line1'    , 'gfit_comp_Gauss'    
  gfit_add_comp, 'line2'    , 'gfit_comp_Gauss'    

  ;;Model expression
  gfit.obs.(0).expr = 'continuum + line1 + line2'

  ;;Secondary expressions to be plotted
  gfit_add_aux, 'plot_cont' , 'continuum'
  gfit_add_aux, 'plot_line1', 'continuum + line1'
  gfit_add_aux, 'plot_line2', 'continuum + line2'

  ;;Guess parameters
  gfit.comp.continuum.par.par.val  = 1.
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

  ;;Setup plot options
  gfit.obs.(0).aux.plot_cont.plot.gp  = 'w l dt 2 lw 2 lc rgb "red"'
  gfit.obs.(0).aux.plot_line1.plot.gp = 'w l lc rgb "blue"'
  gfit.obs.(0).aux.plot_line2.plot.gp = 'w l lc rgb "dark-green"'

  ;;Plot results
  gfit_plot        &  ggp
  gfit_plot_resid  &  ggp
END
