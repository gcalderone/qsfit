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
;  gfit_add_comp
;
;PURPOSE:
;  Add a new component to the GFIT model.
;
;DESCRIPTION:
;  "Components" are the basic blocks of GFIT models, and are defined
;  in gfit_comp_*.pro files as simple IDL functions (see
;  gfit_comp_example).  Components can be combined to build complex
;  models using an IDL mathematical expression, in which the
;  individual components are referred to with their names.  Components
;  are stored in GFIT.comp.
;
;PARAMETERS:
;  LABEL (input, a scalar string)
;    The name of the component.  This will be the name of a field in
;    the gfit.comp structure, hence it ust be a valid IDL name.  Also,
;    it should not clash with other component names.
;
;  COMP (optional input, a scalar string or a structure returned by
;  gfit_component)
;    The type of component to be added to the model.  This parameter
;    can be either the name of a component or a structure returned by
;    gfit_component.  If a string is provided, gfit_component will be
;    called automatically to obtain the corresponding structue.  If
;    COMP is not given the "simplepar" component will be added.
;
;NOTES:
;  The model must be recompiled (using gfit_compile) after a call to
;  to this procedure.
;
PRO gfit_add_comp, label, _comp
  COMPILE_OPT IDL2
  ON_ERROR, !glib.on_error
  COMMON GFIT

  IF (gn(_comp) EQ 0) THEN $
     comp = gfit_component('gfit_comp_simplepar') $
  ELSE BEGIN
     ;;If COMP is a string generate corresponding component structure
     IF (gtype(_comp) EQ 'STRING') THEN   $
        comp = gfit_component(_comp) $
     ELSE $
        comp = _comp
  ENDELSE

  FOR i=0, N_TAGS(comp.par)-1 DO $
     comp.par.(i).comp = label

  IF (gtype(gfit.comp) NE 'STRUCT') THEN $
     tmp = CREATE_STRUCT(label, comp) $
  ELSE $
     tmp = CREATE_STRUCT(gfit.comp, label, comp)
  gfit = {opt: gfit.opt, comp: tmp, obs: gfit.obs, res: gfit.res}
END
