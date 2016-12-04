
;=====================================================================
;NAME:
;  mpfit_eval_model
;
;PURPOSE:
;  Evaluate the model by calling gfit_eval_* for all datasets, and
;  store the result in "gfit.cmp".
;
;PARAMETERS:
;  PAR  (input, array of floating point numbers)
;    The parameter values to evaluate the model.
;
;  EXPR= (output, structure)
;    Passed back from gfit_eval_*.
;
;NOTES:
;  The IDL code for this function will be written by gfit_compile,
;  hence the default implementation simply raises an error.
;
FUNCTION mpfit_eval_model, par, EXPR=expr
  MESSAGE, 'No GFIT model has been compiled.'
END



;=====================================================================
;NAME:
;  gfit_eval_*
;
;PURPOSE:
;  Evaluate the model for a single data set (the "*" in the function
;  name is the corresponding data set name).
;
;PARAMETERS:
;  X (input, array of floating point numbers)
;    The independent values where the model will be evaluated.
;
;  PAR  (input, array of floating point numbers)
;    The parameter values to evaluate the model.
;
;  EXPR= (output, structure)
;    TODO
;
;NOTES:
;  There is no default implementation for this function.  Its IDL code
;  will be written by gfit_compile.
;


;=====================================================================
;NAME:
;  gfit_compile
;
;PURPOSE:
;  Compile current model by writing a .pro file with the IDL code for
;  the gfit_eval_* and mpfit_eval_model functions.
;
;DESCRIPTION:
;  This procedure generates the IDL code necessary to evaluate the
;  currently defined model.  In particular, it generates the code for
;  the mpfit_eval_model function which will be used by MPFIT to
;  evaluate the model while changing the parameter value, resulting in
;  a very fast fitting process.
;
;PARAMETERS:
;  NONE
;
;NOTES:
;  The model must be recompiled each time the user add a component,
;  change the model expression, enable/disable a component and
;  so on.
;
PRO gfit_compile
  COMPILE_OPT IDL2
  ON_ERROR, !glib.on_error
  COMMON GFIT_PRIVATE
  COMMON GFIT

  IF (gfit.comp.nn EQ 0) THEN $
     MESSAGE, 'No component in the model.'

  IF (gfit.comp.npar NE  gn( gfit_get_par() )) THEN $
     MESSAGE, 'Unexpected error'

  ;;Get data set and component names
  detn   = (TAG_NAMES(gfit.data))[0:gfit.data.nn-1]
  cnames = (TAG_NAMES(gfit.comp))[0:gfit.comp.nn-1]

  ;;There must be a model expression for each data set
  FOR idata=0, gfit.data.nn-1 DO BEGIN
     IF (STRTRIM(gfit.expr.(idata).model, 2) EQ '') THEN $
        MESSAGE, 'No MODEL expression defined for data set: ' + detn[idata]
  ENDFOR

  ;;Update the CMP structure
  gfit_prepare_cmp

  ;;Open .pro file for writing
  OPENW, lun, 'mpfit_eval_model' + gn2s(gfit.opt.pid) + '.pro', /get_lun

  ;;Loop through data sets
  FOR idata=0, gfit.data.nn-1 DO BEGIN
     ;;---------------------------------------------------------------
     ;;Write gfit_eval_* function for current data set
     PRINTF, lun, 'FUNCTION gfit_eval_' + detn[idata] + ', x, par, EXPR=_expr'
     PRINTF, lun, '  COMPILE_OPT IDL2'
     PRINTF, lun, '  ON_ERROR, !glib.on_error'
     PRINTF, lun, '  COMMON gfit'
     PRINTF, lun, '  COMMON gfit_private'
     PRINTF, lun, '  currentDataSet = ' + gn2s(idata)
     PRINTF, lun
  
     ;;Loop though components
     npar = 0 ;;account for already considered parameters
     FOR icomp=0, gfit.comp.nn-1 DO BEGIN
        cc = gfit.comp.(icomp)
        IF (cc.enabled) THEN BEGIN
           ;;Evaluate component values for each X
           aa = '  ' + cnames[icomp] + ' = ' + cc.funcName + '(x'
           IF (cc.npar GE 1) THEN BEGIN
              tmp = 'par[' + gn2s(INDGEN(cc.npar) + npar) + ']'
              aa += ', ' + STRJOIN(tmp, ', ')
           ENDIF
           IF (cc.hasopt) THEN $
              aa += ', _extra=gfit.comp.' + cnames[icomp] + '.opt'
           aa += ')'
           PRINTF, lun, aa
        ENDIF $
        ELSE BEGIN
           ;;Component is disabled, use "disabled_val" value.
           PRINTF, lun, '  ' + cnames[icomp] + ' = ' + gn2s(cc.disabled_val)
        ENDELSE

        npar += cc.npar
     ENDFOR
     PRINTF, lun
     
     ;;Model evaluation
     PRINTF, lun, '  MODEL = ' + gfit.expr.(idata).MODEL
     PRINTF, lun

     ;;Secondary expression evaluation
     PRINTF, lun, '  IF (flag_evalAllExpr) THEN BEGIN'
     expr_label = TAG_NAMES(gfit.expr.(idata))
     
     FOR i=1, gn(expr_label)-1 DO $
        PRINTF, lun, '    ' + expr_label[i] + ' = ' + gfit.expr.(idata).(i)
        
     tmp = expr_label + ': ' + expr_label
     PRINTF, lun, '    _expr = { ' + STRJOIN(tmp, ', ') + '}'
     PRINTF, lun, '  ENDIF'
     PRINTF, lun

     ;;Return value
     PRINTF, lun, '  RETURN, MODEL'
     PRINTF, lun, 'END'
     PRINTF, lun
     PRINTF, lun
     PRINTF, lun
  ENDFOR
  
  ;;------------------------------------------------------------------
  ;;Write mpfit_eval_model function
  PRINTF, lun, 'FUNCTION mpfit_eval_model, par, EXPR=expr'
  PRINTF, lun, '  COMPILE_OPT IDL2'
  PRINTF, lun, '  ON_ERROR, !glib.on_error'
  PRINTF, lun, '  COMMON gfit'
  PRINTF, lun, '  COMMON gfit_private'
  PRINTF, lun
  PRINTF, lun, '  flag_evalAllExpr = ARG_PRESENT(expr)'

  FOR idata=0, gfit.data.nn-1 DO $
     PRINTF, lun, '  gfit.cmp.' + detn[idata] + '.m = ' $
             + 'gfit_eval_' + detn[idata] + $
             '(gfit.data.' + detn[idata] + '.x[dataNoticed.('+gn2s(idata)+')], par, EXPR=expr_' + detn[idata] + ')'
  PRINTF, lun

  PRINTF, lun, '  IF (flag_evalAllExpr) THEN BEGIN'
  PRINTF, lun, '    expr = { ' + STRJOIN(detn + ': expr_' + detn, ', ') + '}'
  PRINTF, lun, '  ENDIF'
  PRINTF, lun

  FOR idata=0, gfit.data.nn-1 DO $
     PRINTF, lun, '  wdev_' + detn[idata] + ' = gfit_weighted_dev_' + gfit.opt.data_type + $
             '( gfit.cmp.' + detn[idata] + ' )'
  PRINTF, lun, '  RETURN, [' + STRJOIN('wdev_' + detn, ', ') + ']'
  PRINTF, lun, 'END'
  PRINTF, lun
  PRINTF, lun
  PRINTF, lun

  PRINTF, lun, 'PRO ' + 'mpfit_eval_model' + gn2s(gfit.opt.pid)
  PRINTF, lun, 'END'
  PRINTF, lun

  ;;------------------------------------------------------------------
  ;;Close file and compile it
  FREE_LUN, lun
  backup_quiet = !QUIET
  !QUIET = 1
  RESOLVE_ROUTINE, 'mpfit_eval_model' + gn2s(gfit.opt.pid), /COMPILE_FULL_FILE, /EITHER
  !QUIET = backup_quiet

  ;;Try the newly created routines
  par = gfit_get_par()
  tmp = gfit.opt.log_iter ;;disable logging
  gfit.opt.log_iter = 0

  ;;Ensure there is at least one free parameter, otherwise the routine
  ;;can not be tested
  IF (MIN(par.fixed) EQ 1) THEN $
     MESSAGE, 'All parameters are fixed, can not evaluate the model'

  pval = MPFIT('mpfit_eval_model', par.val, parinfo=par, maxiter=0, iterproc='gfit_iterproc')
  gfit.opt.log_iter = tmp

  ;;If one or more parameters are tied MPFIT will evaluate the
  ;;expressions and return the correct values.  However, these values
  ;;are not yet stored in GFIT structure, hence I do it now:
  gfit_set_parval, pval, /only_tied

  ;;File is no longer needed 
  FILE_DELETE, 'mpfit_eval_model' + gn2s(gfit.opt.pid) + '.pro', /allow
END
