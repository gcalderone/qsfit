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
;  gfit_get_par
;
;PURPOSE:
;  Return all model parameters info.
;
;PARAMETERS:
;  COMP_ENABLED  (output, array of type BYTE)
;    Each element is 1 if the corresponding parameter in the output
;    array belongs to an enabled component, 0 otherwise.
;
;RETURN VALUE: (array of structures)
;  Each element of the array contain information for a model
;  parameter.  The template structure for each element is
;  template_param (see gfit_init.pro).
;
FUNCTION gfit_get_par, comp_enabled
  COMPILE_OPT IDL2
  ON_ERROR, !glib.on_error
  COMMON GFIT

  IF (gfit.comp.npar EQ 0) THEN RETURN, []

  par = REPLICATE(gfit.comp.(0).(0), gfit.comp.npar)
  comp_enabled = REPLICATE(0b, gfit.comp.npar)
  count = 0
  FOR i=0, gfit.comp.nn-1 DO BEGIN
     FOR j=0, gfit.comp.(i).npar-1 DO BEGIN
        par[count] = gfit.comp.(i).(j)
        comp_enabled[count] = gfit.comp.(i).enabled
        count += 1
     ENDFOR
  ENDFOR
  par.limited = FINITE(par.limits)
  IF (gsearch(~comp_enabled, i)) THEN par[i].fixed = 1
  IF (count NE gfit.comp.npar) THEN STOP


  ;;Compile TIE expression
  ;;
  ;;NOTE: the index reported by gfit_report in the TIE column
  ;;corresponds to the first column (#) ONLY when the keyword /ALL is
  ;;given.
  parn = STRUPCASE(par.comp + '.' + par.parname)
  FOR i=0, gn(par)-1 DO BEGIN
     IF (par[i].tied NE '') THEN BEGIN
        tie = par[i].tied

        FOR j=0, gn(parn)-1 DO BEGIN
           WHILE (1) DO BEGIN 
              k = STRPOS(STRUPCASE(tie), parn[j])
              IF (k EQ -1) THEN BREAK
              tie = STRMID(tie, 0, k) + 'p[' + gn2s(j) + ']' + STRMID(tie, k + STRLEN(parn[j]))
           ENDWHILE
        ENDFOR
        par[i].tied = tie
     ENDIF
  ENDFOR

  RETURN, par
END
