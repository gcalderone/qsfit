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
;  gfexists
;
;PURPOSE:
;  Check whether a file exists
;
;PARAMETERS:
;  FILE (input, a scalar string)
;    File name to be checked.
;
;RETURN VALUE: (either 0 or 1)
;  1 if the file exists, 0 otherwise.
;
FUNCTION gfexists, file
  COMPILE_OPT IDL2
  ON_ERROR, !glib.on_error
  
  IF (gn(file) EQ 0) THEN RETURN, 0
  fi = FILE_INFO(file)
  RETURN, fi.exists
END
