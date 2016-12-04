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

;;Prepare fake data:  model is a flat continuum and two emission line
PRO gfit_ex_fakedata, x, y, e
  COMPILE_OPT IDL2
  ON_ERROR, !glib.on_error
  
  n = 200 ;;Number of points
  
  x = ggen(-3, 3, n) ;;independent variable

  y = 2.                  ;;Continuum
  y += ggauss(x, 0, 0.5)  ;;emission line
  y += ggauss(x, 1, 0.15) ;;emission line


   ;;Add noise
  e = 0.1
  y += RANDOMN(seed, n) * e
  e  = REPLICATE(e, n)
END
