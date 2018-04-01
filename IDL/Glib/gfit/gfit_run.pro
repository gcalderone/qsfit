;=====================================================================
;NAME:
;  gfit_iterproc
;
;PURPOSE:
;  Provides feeback while the fit proceeds. This is the function name that
;  will be passed to MPFIT, ITERPROC= keyword.  
;
;NOTES:
;  See MPFIT documentation for further info.
;
PRO gfit_iterproc, myfunct, p, iter, fitstat, DOF=dof, _EXTRA=e
  COMPILE_OPT IDL2
  ON_ERROR, !glib.on_error
  COMMON MPFIT_ERROR, ERROR_CODE
  COMMON GFIT

  IF (~gfit.opt.log_iter) THEN RETURN

  PRINT, FORMAT=gcfmt('\rGFIT: FS/dof: %14.5g/%d   --  iter: %d') $
         , fitstat, dof, iter
END



;=====================================================================
;NAME:
;  gfit_run
;
;PURPOSE:
;  Perform model fitting by by calling MPFIT.
;
;PARAMETERS:
;  NONE
;
;NOTES:
;
PRO gfit_run, EVAL=eval
  COMPILE_OPT IDL2
  ON_ERROR, !glib.on_error
  COMMON GFIT

  ;;Measure elapsed time
  time = SYSTIME(1)

  IF (PTR_VALID(gfit.res.covar)) THEN BEGIN
     PTR_FREE, gfit.res.covar
     gfit.res.covar = PTR_NEW()
  ENDIF     

  IF (KEYWORD_SET(eval)) THEN BEGIN
     gfit.opt.evalAux = 1

     par = gfit_get_par()
     IF (gn(par) EQ 0) THEN $
        MESSAGE, 'No free parameters in the model'
     IF (~gsearch(par.tied EQ '', i)) THEN $
        MESSAGE, 'All parameters are tied'
     par = par[i]

     dummy = mpfit_eval_model(par.val)
     gfit.res.fit_stat = TOTAL(gfit_wdev^2.d)

     gfit.res.test_stat = gfit.res.fit_stat
     gfit.res.test_dof  = gn(gfit_wdev) - gn(WHERE(par.fixed EQ 0   AND  par.tied EQ '', /null))
     gfit.res.test_prob = 1 - CHISQR_PDF(gfit.res.fit_stat, gfit.res.test_dof)
     RETURN
  ENDIF

  maxiter = 1e5
  gfit.opt.evalAux = 0
  gfit.res.mpfit_status = 0
  gfit.res.elapsed_time = gnan()

  par = gfit_get_par()
  IF (~gsearch(par.tied EQ ''  AND  par.fixed EQ 0)) THEN BEGIN
     PRINT, 'WARNING: No free parameters in the model'
     RETURN
  ENDIF

  IF (~gsearch(par.tied EQ '', i)) THEN $
     MESSAGE, 'All parameters are tied'
  par = par[i]

  pval = MPFIT('mpfit_eval_model'                              $
               , ITERPROC='gfit_iterproc'                      $
               , par.val, PARINFO=par, PERROR=perr             $
               , BEST_FJAC=BEST_FJAC, /CALC_FJAC               $
               , COVAR=covar, BESTNORM=fitstat, DOF=dof        $
               , NITER=niter1, STATUS=status, ERRMSG=errmsg    $
               , MAXITER=maxiter                               $
               , FTOL=gfit.opt.tol, XTOL=gfit.opt.tol          $
               , NPRINT=1)
  gfit.res.mpfit_status = status
  ;;gps, par
  ;;print, pval, status
  ;;gkey

  IF (status EQ 5) THEN $
     MESSAGE, 'The maximum number of iterations has been reached'

  ;;Ensure we print a newline since iterpoc only prints carriage return
  IF (gfit.opt.log_iter) THEN PRINT

  IF (status LE 0) THEN BEGIN        
     IF (status EQ -999) THEN BEGIN
        gprint, 'GFIT interrupted by user'
     ENDIF $
     ELSE BEGIN
        gprint
        gprint, 'MPFIT Status: ', status
        gprint, 'Routine mpfit reported an error: '
        gprint, errmsg
        gprint
        gprint, 'Parameter values in last iteration:'
        gprint, pval
        MESSAGE, 'Routine mpfit reported an error: ' + errmsg
     ENDELSE
  ENDIF

  ;;Save parameter's values and errors
  gfit_set_parval, pval, perr
  gfit.res.fit_stat = fitstat

  gfit.res.test_stat = gfit.res.fit_stat
  gfit.res.test_dof  = dof
  gfit.res.test_prob = 1 - CHISQR_PDF(gfit.res.fit_stat, gfit.res.test_dof)

  gfit.res.covar = PTR_NEW(covar)

  gfit.res.elapsed_time = SYSTIME(1)-time
END
