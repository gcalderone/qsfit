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
;  gprint_mgr
;
;PURPOSE:
;  Control the behaviour of "gprint".
;
;PARAMETERS:
;  FILE= (optional input, a scalar string)
;    Name of the file to open for output.  Implies use_file=1.
;
;  /APPEND (keyword)
;    Append (instead of truncating) to the file specified with FILE=.
;
;  /CLOSE (keyword)
;    Close currently opened file. Implies use_file=0.
;
;  USE_STDOUT= (optional input, either 0 or 1)
;    Ensable/disable printing on standard output.
;
;  USE_FILE= (optional input, either 0 or 1)
;    Ensable/disable printing on standard currently opened file.
;
;SEE ASO:
;  utils/gprint.pro
;
PRO gprint_mgr, FILE=filename, APPEND=append, CLOSE=close $
                , USE_STDOUT=kw_stdout, USE_FILE=kw_file
  COMPILE_OPT IDL2
  ON_ERROR, !glib.on_error
  COMMON COM_GPRINT_MGR, use_stdout, use_lun, current_lun

  ;;Initialization
  IF (N_ELEMENTS(current_lun) EQ 0) THEN BEGIN
     current_lun  = -1
     use_stdout = 1
     use_lun    = 0
  ENDIF


  ;;Open file
  IF (KEYWORD_SET(filename)) THEN BEGIN
     IF (current_lun NE -1) THEN BEGIN
        MESSAGE, /info, 'File already opened, closing previous one...'
        FREE_lun, current_lun
     ENDIF

     IF (gtype(filename) EQ 'STRING') THEN $
        OPENW, current_lun, filename, /get_lun, APPEND=append $
     ELSE $
        MESSAGE, 'Filename must be a string'
  ENDIF $
  ELSE BEGIN
     ;;Close file
     IF (KEYWORD_SET(close)) THEN BEGIN
        IF (current_lun NE -1) THEN BEGIN
           FREE_lun, current_lun
           current_lun = -1
           use_lun = 0
        ENDIF
     ENDIF
  ENDELSE

  ;;Enable/Disable print destinations
  IF (N_ELEMENTS(kw_stdout) EQ 1) THEN BEGIN
     use_stdout = KEYWORD_SET(kw_stdout)
  ENDIF

  IF (N_ELEMENTS(kw_lun) EQ 1) THEN BEGIN
     use_lun = KEYWORD_SET(kw_file)
  ENDIF
  IF (KEYWORD_SET(filename)) THEN $
     use_lun = 1

  IF (current_lun EQ -1) THEN use_lun = 0
END

