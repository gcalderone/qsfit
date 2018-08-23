; *******************************************************************
; Copyright (C) 2016-2018 Giorgio Calderone
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

  IF ((gn(funcName) NE 1)  OR  (gtype(funcName) NE 'STRING')) THEN $
     MESSAGE, 'FUNCNAME must be a scalar string'

  ;;Compile component function(s)
  RESOLVE_ROUTINE, funcName, /COMPILE_FULL_FILE, /EITHER

  ;;Get routine parameters and keywords
  gpro_par_key, funcName, par, key

  ;;First parameter is X, skip it
  IF (gn(par) EQ 1) THEN $
     par = [] $ 
  ELSE $
     par = par[1:*]

  ;;Collect parameter substructures
  tmp = {comp:    ''          , $    ;;Name of the component.
         parname: ''          , $    ;;Name of the parameter.
         val:      0.         , $    ;;Best fit value.
         err:      0.         , $    ;;Uncertainty.
         limited: [0b, 0b]    , $    ;;For MPFIT internal use.
         limits:  gnan()*[1,1], $    ;;Lower and upper limits for the parameter value (NaN means no limit is applied).
         fixed:    0b         , $    ;;Whether parametr is fixed (1) or free (0) during the fitting process.
         step:     0.         , $    ;;The minimum step between MPFIT iterations.
         expr:     ''         , $    ;;Tie expression: an IDL mathematical expression to tie this parameter with other ones Parameters are specified as COMPONENT_NAME.PARAMETER_NAME.
         actual:   0.           $    ;;Evaluated value (either the result of expr or the parameter value itself)
        }
  ppar = {}
  FOR i=0, gn(par)-1 DO BEGIN
     ppar = CREATE_STRUCT(ppar, par[i], tmp)
     ppar.(i).parname = par[i]
  ENDFOR
  IF (gn(ppar) EQ 0) THEN ppar = 0

  ;;Prepare the COMP structure
  comp = {funcName: funcName, $    ;;Name of the component IDL function.
          npar:     gn(par) , $    ;;Number of parameters.
          enabled:        1b, $    ;;Enable (1) or disable (0) the component.  When the component is disabled it value is given by "disabled_val".
          disabled_val:   0., $    ;;Value to use when the component is disabled (see "enabled").
          par: ppar         , $
          hasOpt: 0         , $
          hasCdata: 0         $
         }

  ;;Call user defined initialization routine
  IF (groutineExists(funcName + '_init')) THEN $
     CALL_PROCEDURE, funcName + '_init', comp

  IF (groutineExists(funcName + '_opt')) THEN BEGIN
     comp.hasOpt = 1
     comp = gstru_insert(comp, 'opt', CALL_FUNCTION(funcName + '_opt', comp))
  ENDIF

  IF (groutineExists(funcName + '_cdata')) THEN BEGIN
     comp.hasCdata = 1
  ENDIF

  RETURN, comp
END
