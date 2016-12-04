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

;
;This file shows how to implement a custom GFIT model
;component, named "gfit_comp_example".  To implement a component with
;a different name the user should change all the occurrence of
;"gfit_comp_example" with the desired name.
;
;A GFIT component is actually an IDL function fullfilling the
;following requirements:
;
;The code must be stored in a file named "gfit_comp_example.pro".
;This file should be either reachable through one of the directories
;listed in the !PATH system variable, or be already compiled when the
;component is used.
;
;The function must accept at least one parameter, i.e. the X value
;where the model is to be evaluated.
;
;The function may accepts further parameters (p1 and p2 in this
;case), which would be the actual component parameters to be varied
;until a good fit is reached.
;
;The function may also accepts keywords (opt1 and opt2 n this case),
;actually the component options, i.e. fixed values not changed during
;the fitting process.  The component options may be of any type, even
;not numerical.
;
;The function must return either a scalar or an array of floating
;point numbers.  If an array is returned it must have the same number
;of elements of the input parameter X.
;
FUNCTION gfit_comp_example $
   , x                     $ ;;Data set independent variable
   , p1, p2                $ ;;Component parameters (floating point scalar values)
   , OPT1=opt1, OPT2=opt2    ;;Component options
  
  RETURN, x * 0.
END

;
;The component parameters, options and internal data may be
;initialized with a procedure named "gfit_comp_example_init".  This
;procedure must accepts one argument, actually a structure whose tag
;names are the same as the parameters accepted by
;"gfit_comp_example". There is a further tag named "opt" which allows
;to access the component options.
;
;Each tag corresponding to a parameter will be a structure like
;"template_param" (see gfit.init.pro).
;
;Each tag corresponding to a keyword will be either the value returned
;by the associated function (see gfit_comp_example_opt1) or an integer
;value.
;
PRO gfit_comp_example_init, comp
  comp.p1.val       = 0
  comp.p2.fixed     = 1
  comp.opt.opt1.aa  = 2
  comp.opt.opt2     = 3
END

;
;The component options, such as "opt1" and "opt2" may be of any type,
;even not numerical, and a default value can be provided by a specific
;function returning the default value.  For instance, to initialize
;the "opt1" option the name of the function should be
;"gfit_comp_example_opt1", and the implementation should be as
;follows:
FUNCTION gfit_comp_example_opt1
  RETURN, { aa: 0., bb: 'Option 2', cc: { dd: 0. } }
END

;If there is not an initializing function for an option the default
;value will be 0 (an integer value).


;The component routines may be tested using the "gfit_component"
;function, which will prepare the component to be used in the GFIT
;framework, e.g.:
;
;  comp = gfit_component("gfit_comp_example")
;  HELP, comp
;  PRINT, CALL_FUNCTION(comp.funcName, 1, 2, 3, OPT2=4)
;
