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
;  gfit_common
;
;PURPOSE:
;  Define GFIT common blocks and the template structures.
;
;PARAMETERS:
;  NONE.
;
PRO gfit_common
  COMMON GFIT_PRIVATE, flag_evalAllExpr, currentDataSet, $
     dataNoticed, $
     template_res, template_opt, template_main_plotopt,  $
     template_expr_plotopt, template_comp, template_param
  COMMON GFIT, gfit

  ;;The template structure for the "res" field in GFIT structure.
  template_res = $
     { fit_stat:  gnan(), $ ;;Fit statistic
       test_stat: gnan(), $ ;;Test statistic
       test_dof:  0l    , $ ;;Test degrees of freedom
       test_prob: gnan(), $ ;;Test probability
       mpfit_status: 0l , $ ;;MPFIT status
       elapsed_time: 0. , $ ;;Fitting elapsed time
       covar:      gnan() $ ;;Covariance matrix
     }
  
  template_opt = $
     { pid:        0l    , $ 
       tol:        1.e-6 , $
       data_type: 'gauss', $ ;;Statistic appropriate for data, either 'gauss' or 'poisson'
       gof_test:   1b    , $ ;;perform Goodness-of-fit test (i.e. call gfit_teststat_*)
       log_iter:   1b      $ ;;Log fit progress in gfit_iterproc
     }

  ;;The template structure for the main (i.e. global) plotting options.
  template_main_plotopt = $
     {rebin: 1l            , $ ;;Rebin data for plotting (disabled if <= 1)
      xlog:  0b            , $ ;;Enable (1) or disable (0) the logarithmic X axis.
      ylog:  0b            , $ ;;Enable (1) or disable (0) the logarithmic Y axis.
      xr:    gnan() * [1,1], $ ;;Range for the X axis.  Use NaN to use autoscale.
      yr:    gnan() * [1,1], $ ;;Range for the Y axis.  Use NaN to use autoscale.
      title: 'notitle'     , $ ;;Plot title.
      xtit:  'X'           , $ ;;Label for X axis.
      ytit:  'Y'             $ ;;Label for Y axis.
     }

  ;;The template structure to plot an expression.
  template_expr_plotopt = $
     {plot:     1b, $ ;;Enable (1) or disable (0) the plot of this expression.
      label:    '', $ ;;Label shown in plot legend.
      gp:       ''  $ ;;Gnuplot format
     }
  
  ;;The template structure for a component.
  template_comp = $
     {name:         '', $ ;;Component name.
      funcName:     '', $ ;;Name of the component IDL function.
      npar:         0 , $ ;;Number of parameters.
      enabled:      1b, $ ;;Enable (1) or disable (0) the component.  When the component is disabled it value is given by "disabled_val".
      disabled_val: 0., $ ;;Value to use when the component is disabled (see "enabled").
      hasopt:       0b  $ ;;Whether the component has options (i.e. keywords to the IDL function).
     }
  
  ;;The template structure for each parameter in a component.
  template_param = $
     {comp:    ''          , $ ;;Name of the component.
      parname: ''          , $ ;;Name of the parameter.
      val:      0.         , $ ;;Best fit value.
      err:      0.         , $ ;;Uncertainty.
      limited: [0b, 0b]    , $ ;;For MPFIT internal use.
      limits:  gnan()*[1,1], $ ;;Lower and upper limits for the parameter value (NaN means no limit is applied).
      fixed:    0b         , $ ;;Whether parametr is fixed (1) or free (0) during the fitting process.
      tied:     ''         , $ ;;Tie expression: an IDL mathematical expression to tie this parameter with other ones Parameters are specified as COMPONENT_NAME.PARAMETER_NAME.
      step:     0.           $ ;;The minimum step between MPFIT iterations.
     }
END



;=====================================================================
;NAME:
;  gfit_set_parval
;
;PURPOSE:
;  Save the parameter values into the GFIT structure.
;
;PARAMETERS:
;  PVAL  (input, array of floating point numbers)
;    New values to be stored in the GFIT structure.  There must be a
;    value for each parameter in the model.
;
;  PERR  (input, array of floating point numbers)
;    New uncertainties values to be stored in the GFIT structure.
;    There must be a value for each parameter in the model.
;
;  /ONLY_TIED (keyword)
;    If given only the tied parameters will be affected.  Note that
;    the PVAL and PERR array length must always be the same,
;    regardless of the /ONLY_TIED keyword.
;
PRO gfit_set_parval, pval, perr, ONLY_TIED=only_tied
  COMPILE_OPT IDL2
  ON_ERROR, !glib.on_error
  COMMON GFIT

  only_tied = KEYWORD_SET(only_tied)
  IF (N_PARAMS() LT 2) THEN perr = REPLICATE(gnan(), gn(pval))

  k = 0
  FOR i=0, gfit.comp.nn-1 DO BEGIN
     FOR j=0, gfit.comp.(i).npar-1 DO BEGIN
        par = gfit.comp.(i).(j)

        IF ((par.tied NE '')   OR   $
            (~only_tied)) THEN BEGIN

           par.val  = pval[k]
           par.err  = perr[k]

           ;;Check param value is within the limit
           IF ((par.val EQ par.limits[0])   OR   $
               (par.val EQ par.limits[1]))  THEN BEGIN
              par.err  = gnan()
           ENDIF
        ENDIF

        gfit.comp.(i).(j) = par
        k += 1
     ENDFOR
  ENDFOR
END








;=====================================================================
;NAME:
;  gfit_init
;
;PURPOSE:
;  Initialize GFIT status.
;
;PARAMETERS:
;  NONE
;
PRO gfit_init
  COMPILE_OPT IDL2
  ON_ERROR, !glib.on_error
  COMMON GFIT_PRIVATE
  COMMON GFIT

  gfit_common
  empty_struct = { __: 0b }

  gfit = { $
           opt:   template_opt         $
         , data:  { nn: 0 }            $
         , comp:  { nn: 0, npar: 0 }   $
         , expr:  empty_struct         $
         , cmp:   empty_struct         $
         , res:   template_res         $
         , plot:  empty_struct         $
         }
  gfit.opt.data_type = 'gauss'
END
