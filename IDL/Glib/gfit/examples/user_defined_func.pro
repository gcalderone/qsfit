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

FUNCTION user_defined_func, x, continuum, norm1, center1, sigma1, norm2, center2, sigma2
  COMPILE_OPT IDL2
  ON_ERROR, !glib.on_error

  RETURN, continuum $
          + norm1 * EXP(-(x - center1)^2 / (2 * sigma1^2)) / SQRT(2*!PI) / sigma1 $
          + norm2 * EXP(-(x - center2)^2 / (2 * sigma2^2)) / SQRT(2*!PI) / sigma2
END
