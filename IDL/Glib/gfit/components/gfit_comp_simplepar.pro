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
;GFIT MODEL COMPONENT
;
;NAME:
;  gfit_comp_simplepar
;
;COMPONENT DESCRIPTION:
;  A scalar parameter in the GFIT model. It only has one parameter
;  which is the actual component value.
;
;PARAMETERS:
;  PAR (units: [Y])
;    The actual component value.
;
;OPTIONS:
;  NONE
;

FUNCTION gfit_comp_simplepar, x, par
  RETURN, par
END


