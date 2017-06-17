; *******************************************************************
; Copyright (C) 2016-2017 Giorgio Calderone
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
;  glib_init
;
;PURPOSE:
;  Initialize the GLIB library.
;
;PARAMETERS:
;  PATH (input, a scalar string)
;    Path to the GLIB directory.  If this parameter is not given the path
;    where this routine is stored will be considered.
;
PRO glib_init, path
  IF (N_PARAMS() EQ 0) THEN $
     path = FILE_DIRNAME(ROUTINE_FILEPATH('glib_init')) + PATH_SEP()

  ;;Initialize !GLIB structure
  DEFSYSV, '!GLIB', EXISTS = exists
  IF (exists NE 1) THEN BEGIN 
     glib = {version:    '0.7.1' $
             , path:     path    $
             , on_error: 2b      $
            }
     DEFSYSV, '!GLIB', glib

     PRINT, 'GLIB version: ' + glib.version
  ENDIF

  ;;Initialize sub-packages
  ggp_init
  gfit_init
END
