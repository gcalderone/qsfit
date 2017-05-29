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
;  gpro_par_key
;
;PURPOSE:
;  Returns the list of input parameters and of keywords of a compiled
;  procedure/function.
;
;NOTES:
;  Procedure/function must have been compiled before using this
;  routine.
;
;PARAMETERS:
;  PRO_NAME (input, a scalar string)
;    The name of a procedure/function.
;
;  PAR  (output, array of strings)
;    The names of all input parameters.
;
;  KEY (output, array of strings)
;    The names of all keywords.
;
;  FOUND= (output, either 0 or 1)
;    1 if the procedure/function has been found, 0 otherwise
;
PRO gpro_par_key, pro_name, par, key, FOUND=found
  COMPILE_OPT IDL2
  ON_ERROR, !glib.on_error

  par = []
  key = []
  found = 0

  HELP, names=pro_name, /routines, output=aa

  FOR i=0, gn(aa)-1 DO BEGIN
     ll = STRSPLIT(aa[i], ' ', /extract)
     
     IF (ll[0] EQ STRUPCASE(pro_name)) THEN BEGIN
        IF (gn(ll) GT 1) THEN BEGIN
           ll = ll[1:*] ;;First item is pro/func name
           
           FOR j=0, gn(ll)-1 DO BEGIN
              IF (ll[j] EQ STRLOWCASE(ll[j])) THEN par = [par, ll[j]] $
              ELSE                                 key = [key, ll[j]]
           ENDFOR
        ENDIF

        found = 1
        BREAK
     ENDIF
  ENDFOR
END
