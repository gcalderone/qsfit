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
PRO gfit_run
  COMPILE_OPT IDL2
  ON_ERROR, !glib.on_error
  COMMON GFIT_PRIVATE
  COMMON GFIT

  ;;Measure elapsed time
  time = SYSTIME(1)

  niter = 0
  WHILE (1) DO BEGIN
     par = gfit_get_par()

     pval = MPFIT('mpfit_eval_model'                              $
                  , ITERPROC='gfit_iterproc'                      $
                  , par.val, PARINFO=par, PERROR=perr             $
                  , BEST_FJAC=BEST_FJAC, /CALC_FJAC               $
                  , COVAR=covar, BESTNORM=fs, DOF=dof             $
                  , NITER=niter1, STATUS=status, ERRMSG=errmsg    $
                  , MAXITER=1e5                                   $
                  , FTOL=gfit.opt.tol, XTOL=gfit.opt.tol          $
                  , NPRINT=1)

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

        BREAK
     ENDIF
     niter += niter1

     ;;Save parameter's values and errors
     gfit_set_parval, pval, perr
     
     IF (status EQ 5) THEN BEGIN
        gprint, 'The maximum number of iterations has been reached'
        gprint, 'Would you like to continue (y/n) ?'
        gkey, key=c
        gprint, 'Answer: ', c
        gprint
        IF (STRUPCASE(c) NE 'Y') THEN BREAK
     ENDIF $
     ELSE BREAK
  ENDWHILE

  ;;Evaluate model, "fit" and "test" statistics.
  gfit_eval


  ;;Update RES structure
  res = gstru_sub(gfit.res, drop='covar')
  res = CREATE_STRUCT(res, 'covar', covar)
  res.mpfit_status = status
  res.elapsed_time = SYSTIME(1)-time

  gfit = { $
         opt:   gfit.opt     , $
         data:  gfit.data    , $
         comp:  gfit.comp    , $
         expr:  gfit.expr    , $
         cmp:   gfit.cmp     , $
         res:   res          , $
         plot:  gfit.plot      $
         }
END
