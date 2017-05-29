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
;  gfit_weighted_dev_gauss
;
;PURPOSE:
;  Evaluate the weighted deviations between the model and the data.
;
;PARAMETERS:
;  CMP  (input, structure with the same form as GFIT.CMP)
;    The GFIT.CMP structure to compare model and data.
;
;RETURN VALUE: (array of floating point numbers)
;  An array of weighted deviations between the model and the
;  data.
;
FUNCTION gfit_weighted_dev_gauss, cmp
  COMPILE_OPT IDL2
  ON_ERROR, !glib.on_error

  RETURN, (cmp.y - cmp.m) / cmp.e
END



