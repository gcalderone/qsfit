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

;=====================================================================
;NAME:
;  groutineexists
;
;PURPOSE:
;  Check whether a function or procedure exists
;
;PARAMETERS:
;  NAME (input, a scalar string)
;    An IDL routine name
;
;RETURN VALUE: (either 0 or 1)
;  1 if the routine exists, 0 otherwise.
;
FUNCTION groutineexists, name
  COMPILE_OPT IDL2
  ON_ERROR, !glib.on_error
  
  IF (EXECUTE('RESOLVE_ROUTINE, name, /EITHER, /NO_RECOMPILE', 1, 1)) THEN $
     RETURN, 1

  RETURN, 0
END
