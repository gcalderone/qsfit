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
;  gprint_error
;
;PURPOSE:
;  Prints the error message and the !ERROR_STATE structure of the last
;  issued error, as well as the current call stack, using "gprint,
;  /override".
;
;PARAMETERS:
;  NONE.
;
;USAGE:
;  The gprint_error is typically used in catch blocks, e.g.:
;
;  IF (!glib.on_error EQ 2) THEN BEGIN
;    CATCH, error_status
;    IF (error_status NE 0) THEN BEGIN
;       CATCH, /cancel
;       gprint_error
;       RETURN
;    ENDIF
;  ENDIF
;
;SEE ALSO:
;  utils/gprint.pro
;  utils/gprint_mgr.pro
;
PRO gprint_error
  COMPILE_OPT IDL2
  ON_ERROR, !glib.on_error

  ;;Get call stack info (here I use "HELP, /last_message" instead of
  ;;"SCOPE_TRACEBACK" since the latter is not able to cope with test 5
  ;;at the bottom of this file).
  HELP, /last_message, output=string_trace
  i = WHERE(STRPOS(string_trace, 'Execution halted at:') NE -1)
  IF (i[0] EQ -1) THEN BEGIN
     gprint, /override, string_trace
     RETURN
  ENDIF
  string_trace = string_trace[i:*]

  traceBack = {ROUTINE: '', FILENAME: '', LINE: 0l}
  FOR i=0, N_ELEMENTS(string_trace)-1 DO BEGIN
     s = string_trace[i]
     s = STRMID(s, 23)
     s = STRTRIM(s, 2)
     s = STRSPLIT(s, ' ', /extract)

     IF (N_ELEMENTS(s) EQ 3) THEN BEGIN
        IF (s[0] EQ 'GASSERT') THEN CONTINUE ;;skip gassert level

        traceBack = [traceBack, traceBack[0]]
        traceBack[-1].routine  = s[0]
        traceBack[-1].filename = STRJOIN(s[2:*], ' ')
        traceBack[-1].line     = LONG(s[1])
     ENDIF
  ENDFOR
  traceBack = traceBack[1:*]

  ;;Print structure in arrays of strings
  gps, row=0, traceBack, OUT=traceBack
  gps, row=0, gstru_sub(!ERROR_STATE, drop='MSG'), OUT=errorState

  ;;Print messages using "gprint, /override"
  gprint, /override
  gprint, /override, '===============================  GPRINT_ERROR  ==============================='
  gprint, /override, !ERROR_STATE.MSG
  gprint, /override
  gprint, /override, 'ERROR_STATE:'
  gprint, /override, errorState
  gprint, /override
  gprint, /override, 'IDL call stack:'
  gprint, /override, traceBack
  gprint, /override, '=============================================================================='
END
