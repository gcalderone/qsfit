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
;  gfit_comp_powerlaw
;
;COMPONENT DESCRIPTION:
;  A power law component in the form:
;    NORM * (X / X0)^INDEX
;
;PARAMETERS:
;  NORM (units: [Y])
;    Component value at x=X0.
;
;  X0 (units: [X])
;    Reference X value.  This parameter should always be fixed,
;    otherwise its value would be highly correlated with NORM.
;
;  INDEX (no units)
;    Power law index.
;
;OPTIONS:
;  NONE
;



;=====================================================================
PRO gfit_comp_powerlaw_init, comp
  comp.norm.val  =  1
  
  comp.x0.val    =  1
  comp.x0.fixed  =  1           ;;this is strongly correlated with norm
     
  comp.index.val = -1
END

;=====================================================================
FUNCTION gfit_comp_powerlaw, x, norm, x0, index
  COMPILE_OPT IDL2
  ON_ERROR, !glib.on_error

  ret = norm * (x/x0)^index
  RETURN, ret
END


