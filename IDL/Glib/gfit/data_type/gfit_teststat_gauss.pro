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

;;--------------------------------------------------------------------
PRO gfit_teststat_gauss, cmp
  COMPILE_OPT IDL2
  ON_ERROR, !glib.on_error
  COMMON GFIT

  chisq = TOTAL( ((cmp.y - cmp.m)^2 / cmp.e^2) )
  dof = gn(cmp) - gn(gfit_get_par())

  gfit.res.test_stat = chisq
  gfit.res.test_dof  = dof
  gfit.res.test_prob = 1 - CHISQR_PDF(chisq, dof)
END
