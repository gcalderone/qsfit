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
;  gfit_component
;
;PURPOSE:
;  Return a structure describing a GFIT component.
;
;DESCRIPTION:
;  This function return a structure suitable to be inserted in
;  GFIT.comp.  Components are prepared using specifically designed
;  routines, which should be available in one of the directories
;  listed in the !PATH environment variable or be already compiled
;  when gfit_component is called.  The name of such functions should
;  start with "gfit_comp_".
;
;PARAMETERS:
;  FUNCNAME (input, a scalar string)
;    The name of an IDL function used to evaluate the component
;    result, without the "gfit_comp_" prefix.  See
;    gfit_comp_example.pro for an example.
;
;RETURN VALUE: (a structure)
;  A structure describing a GFIT component.  Such structure may be
;  passed to "gfit_add_comp, type=" to quickly add a component into
;  GFIT.
;
FUNCTION gfit_component, funcName
  COMPILE_OPT IDL2
  ON_ERROR, !glib.on_error
  COMMON GFIT_PRIVATE

  IF ((gn(funcName) NE 1)  OR  (gtype(funcName) NE 'STRING')) THEN $
     MESSAGE, 'FUNCNAME must be a scalar string'

  ;;Compile component function(s)
  RESOLVE_ROUTINE, funcName, /COMPILE_FULL_FILE, /EITHER

  ;;Get routine parameters and keywords
  gpro_par_key, funcName, par, key

  ;;First parameter is input variable
  IF (gn(par) EQ 1) THEN $
     par = [] $ 
  ELSE $
     par = par[1:*]

  ;;Prepare the COMP structure
  comp = template_comp
  comp.name = funcName
  comp.funcName = funcName
  comp.npar = gn(par)

  ;;Collect parameter substructures
  ppar = []
  FOR i=0, comp.npar-1 DO BEGIN
     ppar = CREATE_STRUCT(ppar, par[i], template_param)
     ppar.(i).parname = par[i]
  ENDFOR

  ;;Join all toghether
  comp = CREATE_STRUCT(ppar, comp)

  ;;Prepare option structure.  This will be passed through the _EXTRA=
  ;;keyword
  opt = []
  FOR i=0, gn(key)-1 DO BEGIN
     success = EXECUTE('tmp = ' + funcName + '_' + key[i] + '()', 1, 1)
     IF (~success) THEN tmp = 0

     opt = CREATE_STRUCT(opt, key[i], tmp)
  ENDFOR
  IF (gn(key) GT 0) THEN BEGIN
     comp.hasopt = 1
     comp = CREATE_STRUCT(comp, 'opt', opt)
  ENDIF

  ;;Call user defined initialization routine
  IF (groutineExists(funcName + '_init')) THEN $
     CALL_PROCEDURE, funcName + '_init', comp

  RETURN, comp
END
