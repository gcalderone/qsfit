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

;=====================================================================
;NAME:
;  gassert
;
;PURPOSE:
;  Check input condition(s).  If it is false print current call stack
;  and issue an error.  If the caller routine set ON_ERROR, 0 the
;  execution will stop at the line in which gassert has been called.
;
;PARAMETERS:
;  CONDITION (input, a scalar or array of boolean)
;    Condition(s) to be checked.
;
PRO gassert, condition
  ON_ERROR, 2
  IF (TOTAL(condition) NE N_ELEMENTS(condition)) THEN $
     MESSAGE, 'Assertion failed'
END
