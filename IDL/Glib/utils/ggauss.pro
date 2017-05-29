; *******************************************************************
; Copyright (C) 2016,2017 Giorgio Calderone
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
;  ggauss
;
;PURPOSE:
;  Compute the Gaussian function at given X, with given mean and sigma
;  parameters.
;
;PARAMETERS:
;  X (input, array of numbers)
;   Values at which the Gaussian function is to be evaluated.
;
;  MEAN (input, a scalar number)
;    Mean parameter of the Gaussian function.
;
;  SIGMA  (input, a scalar number)
;    Sigma parameter of the Gaussian function.
;
;RETURN VALUE: (array of numbers)
;  Values of the Gaussian function at given X.
;
FUNCTION ggauss, x, mean, sigma
  COMPILE_OPT IDL2
  ON_ERROR, !glib.on_error

  res = EXP((-(((x-mean) / sigma) ^ 2.) / 2.) > (-80))
  res /= (SQRT(2. * !PI) * sigma)

  RETURN, res
END


