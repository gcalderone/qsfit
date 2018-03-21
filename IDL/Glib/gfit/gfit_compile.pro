
;=====================================================================
;NAME:
;  mpfit_eval_model
;
;PURPOSE:
;  Evaluate the model by calling gfit_eval_* for all datasets, and
;  store the result in "gfit.eval".
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

FUNCTION doEvaluation, i0, nn, par
  COMMON GFIT
  ret = 0
  FOR i=0, nn-1 DO BEGIN
     IF (cachePar[i0+i] NE par[i]) THEN BEGIN
        cachePar[i0+i] = par[i]
        ret = 1
     ENDIF
  ENDFOR
  RETURN, ret
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
  COMMON GFIT

  IF (gtype(gfit.comp) NE 'STRUCT') THEN $
     MESSAGE, 'No component in the model.'

  IF (N_TAGS(gfit.obs) EQ 0) THEN $
     MESSAGE, 'No observations in the model.'

  ;;Update the EVAL structure
  gfit_prepare_eval

  ;;Open .pro file for writing
  OPENW, lun, 'mpfit_eval_model' + gn2s(gfit.opt.pid) + '.pro', /get_lun

  ;;Component names and parameters
  cnames = TAG_NAMES(gfit.comp)
  allPar = gfit_get_par()
  par = allPar

  ;;------------------------------------------------------------------
  ;; Prepare CDATA
  cdataNames = []
  IF (gn(gfit_cdata) GT 0) THEN $
     cdataNames = TAG_NAMES(gfit_cdata)

  FOR icomp=0, N_TAGS(gfit.comp)-1 DO BEGIN
     IF (gfit.comp.(icomp).hasCdata) THEN BEGIN
        FOR iobs=0, N_TAGS(gfit.obs)-1 DO BEGIN
           tag = 'i' + gn2s(iobs) + '_' + cnames[icomp]

           IF (gsearch(STRUPCASE(tag) EQ cdataNames, itag)) THEN BEGIN
              tmp = gfit_cdata.(itag)
              tmp = CALL_FUNCTION(gfit.comp.(icomp).funcName + '_cdata', $
                                  gfit.comp.(icomp), gfit.obs.(iobs).eval.x, tmp)
           ENDIF $
           ELSE BEGIN
              tmp = CALL_FUNCTION(gfit.comp.(icomp).funcName + '_cdata', $
                                  gfit.comp.(icomp), gfit.obs.(iobs).eval.x)
              gfit_cdata = CREATE_STRUCT(gfit_cdata, tag, tmp)
           ENDELSE
        ENDFOR
     ENDIF
  ENDFOR


  ;;------------------------------------------------------------------
  ;;Write mpfit_eval_model function
  PRINTF, lun, 'FUNCTION mpfit_eval_model, par'
  PRINTF, lun, '  COMPILE_OPT IDL2'
  PRINTF, lun, '  ON_ERROR, !glib.on_error'
  PRINTF, lun, '  COMMON gfit'
  PRINTF, lun
  dummy = gsearch(count=tmp, par.tied EQ '')
  PRINTF, lun, '  IF (gn(par) NE ' + gn2s(tmp) + ') THEN BEGIN'
  PRINTF, lun, '    PRINT, gn(par), ', gn2s(tmp)
  PRINTF, lun, '    STOP'
  PRINTF, lun, '  ENDIF'
  ;PRINTF, lun, '  HELP, par'
  PRINTF, lun

  PRINTF, lun, '  ;; Parameters'
  count = 0
  FOR ipar=0, gn(par)-1 DO BEGIN
     IF (par[ipar].tied NE '') THEN CONTINUE
     
     tmp = '  ' + par[ipar].comp + '_' + par[ipar].parname
     PRINTF, lun, tmp + ' = par[' + gn2s(count) + ']'
     count += 1
  ENDFOR

  ;;Drop tied parameters
  IF (~gsearch(par.tied EQ '', i)) THEN $
     MESSAGE, 'ALl parameters are tied'
  par = par[i]

  ;;Loop through observations
  cachePar = []
  FOR iobs=0, N_TAGS(gfit.obs)-1 DO BEGIN
     PRINTF, lun
     PRINTF, lun, '  ;; Obs. ' + gn2s(iobs)
     PRINTF, lun, '  cc = gfit.obs.('+gn2s(iobs)+').eval'
     obs = gfit.obs.(iobs)

     ;;Loop though components
     npar = 0 ;;account for already considered parameters
     FOR icomp=0, N_TAGS(gfit.comp)-1 DO BEGIN
        cc = gfit.comp.(icomp)
        IF (cc.enabled) THEN BEGIN
           IF (gsearch(STRUPCASE(allPar.comp) EQ cnames[icomp]  AND  allPar.tied NE '', ipar)) THEN BEGIN
              tmp1 = allPar[ipar].comp + '_' + allPar[ipar].parname + ' = ' + allPar[ipar].tied
              tmp2 = 'gfit.comp.' + allPar[ipar].comp + '.par.' + allPar[ipar].parname + '.val = ' + allPar[ipar].comp + '_' + allPar[ipar].parname
              PRINTF, lun, '  ' + tmp1
              PRINTF, lun, '  ' + tmp2
           ENDIF

           ;;Evaluate component values for each X
           aa = '  cc.' + cnames[icomp] + ' = ' + cc.funcName + '(cc.x'
           IF (cc.npar GT 0) THEN BEGIN
              tmp = cnames[icomp] + '_' + TAG_NAMES(cc.par)
              aa += ', ' + STRJOIN(tmp, ', ')
              tmp1 = gn2s(npar) + ':' + gn2s(npar + cc.npar-1)
              PRINTF, lun, '  IF (doEvaluation(' + gn2s(npar) + ', ' + gn2s(cc.npar) + ', [' + STRJOIN(tmp, ',') +  '])) THEN $'
           ENDIF
           IF (cc.hasOpt) THEN aa += ', opt=gfit.comp.' + cnames[icomp] + '.opt'
           IF (cc.hasCdata) THEN aa += ', cdata=gfit_cdata.i' + gn2s(iobs) + '_' + cnames[icomp]
           aa += ')'
           PRINTF, lun, '  ' + aa
        ENDIF
        npar += cc.npar
     ENDFOR
     PRINTF, lun
     cachePar = FLTARR(npar)
     
     ;;Model evaluation
     PRINTF, lun, '  cc.m = ' + obs.expr
     IF (N_TAGS(obs.aux) GT 0) THEN BEGIN
        anames = TAG_NAMES(obs.aux)
        PRINTF, lun, '  IF (gfit.opt.evalAux) THEN BEGIN'
        FOR i=0, N_TAGS(obs.aux)-1 DO $
           PRINTF, lun, '    cc.aux.' + anames[i] + ' = ' + obs.aux.(i).expr
        PRINTF, lun, '  ENDIF'
     ENDIF

     PRINTF, lun, '  _wdev'+gn2s(iobs)+' = gfit_weighted_dev_' + gfit.opt.data_type + '(cc)'
     PRINTF, lun, '  gfit.obs.(' + gn2s(iobs) + ').eval = cc'
     PRINTF, lun
  ENDFOR

  tmp = '_wdev' + gn2s(LINDGEN(N_TAGS(gfit.obs)))
  PRINTF, lun, '  gfit_wdev = [' + STRJOIN(tmp, ', ') + ']'
  ;PRINTF, lun, '  i = where(~finite(gfit.obs.(0).eval.m))'
  ;PRINTF, lun, '  IF (i[0] NE -1) THEN STOP'
  PRINTF, lun, '  RETURN, gfit_wdev'
  PRINTF, lun, 'END'
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

  ;;Test the new routines
  gfit_run, /eval

  ;;File is no longer needed 
  FILE_DELETE, 'mpfit_eval_model' + gn2s(gfit.opt.pid) + '.pro', /allow
END
