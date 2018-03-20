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

PRO gfit_ex2
  COMMON GFIT

  ;;Generate fake data 
  gfit_ex_fakedata, x, y, e

  ;;Initialize gfit
  gfit_init

  ;;Add data into gfit
  gfit_add_data, x, y, e

  ;;Add components to model (check user_defined_func.pro)
  gfit_add_comp, 'func', 'user_defined_func'
           
  ;;Model expression
  gfit.obs.(0).expr = 'func'

  ;;Guess parameters
  gfit.comp.func.par.continuum.val = 1.
  gfit.comp.func.par.norm1.val     = 0.5
  gfit.comp.func.par.center1.val   = 0.1
  gfit.comp.func.par.sigma1.val    = 0.5
  gfit.comp.func.par.norm2.val     = 0.5
  gfit.comp.func.par.center2.val   = 0.9
  gfit.comp.func.par.sigma2.val    = 0.2

  ;;Run fit
  gfit_compile
  gfit_run
  gfit_report

  ;;Plot results
  gfit_plot       & ggp
  gfit_plot_resid & ggp
END
