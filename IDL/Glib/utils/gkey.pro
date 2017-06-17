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
;  gkey
;
;PURPOSE:
;  Prompt for user input through keyboard
;
;NOTES:
;  If the KEY= keyword is not present the GKEY routine reacts to the
;  'r' and 's' keys by issuing a RETALL and STOP command
;  respectively.
;
;PARAMETERS:
;  MSG= (optional input, a scalar string)
;    Message printed on standard output.
;
;  KEY= (output, a scalar string)
;    Key pressed by user
;
PRO gKey, MSG=msg, KEY=key
  ON_ERROR, 2

  IF (~KEYWORD_SET(msg)) THEN $
     msg = '                                                                  (press a key)'
  PRINT, msg
  
  key = GET_KBRD()
  
  IF (~ARG_PRESENT(key)) THEN BEGIN
     CASE key OF
        'r': RETALL
        's': BEGIN
           PRINT, 'Stopping execution'
           STOP
        END
        ELSE:
     ENDCASE
  ENDIF
END

