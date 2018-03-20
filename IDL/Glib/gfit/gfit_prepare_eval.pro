;=====================================================================
;NAME:
;  gfit_prepare_eval
;
;PURPOSE:
;  Prepare the EVAL structures according to defined obs(s)
;
;PARAMETERS:
;  NONE
;
PRO gfit_prepare_eval
  COMPILE_OPT IDL2
  ON_ERROR, !glib.on_error
  COMMON GFIT

  IF (N_TAGS(gfit.obs) LT 1) THEN $
     MESSAGE, 'No observation available.'

  ;;Prepare the EVAL structure
  FOR iobs=0, N_TAGS(gfit.obs)-1 DO BEGIN
     obs = gfit.obs.(iobs)
     nn = 0
     FOR idata=0, N_TAGS(obs.data)-1 DO BEGIN
        i = WHERE(obs.data.(idata).group GT 0)
        IF (i[0] EQ -1) THEN $
           MESSAGE, 'No good data on obs. ' + gn2s(iobs) + ', dataset ' + gn2s(idata)
        nn += gn(i)
     ENDFOR


     tmp = { x: FLTARR(nn),  $
             y: FLTARR(nn),  $
             e: FLTARR(nn),  $
             m: FLTARR(nn)   $
           }
     tmp.m = gnan()
     IF (N_TAGS(gfit.comp) GT 0) THEN BEGIN
        cnames = TAG_NAMES(gfit.comp)
        FOR icomp=0, N_TAGS(gfit.comp)-1 DO BEGIN
           val = REPLICATE(DOUBLE(gnan()), nn)
           IF (~gfit.comp.(icomp).enabled) THEN $
              val = gfit.comp.(icomp).disabled_val
           tmp = CREATE_STRUCT(tmp, cnames[icomp], DOUBLE(val))
        ENDFOR
     ENDIF

     aux = {}
     val = REPLICATE(gnan(), nn)
     FOR iaux=0, N_TAGS(obs.aux)-1 DO $
        aux = CREATE_STRUCT(aux, (TAG_NAMES(obs.aux))[iaux], val)
     IF (N_TAGS(aux) EQ 0) THEN $
        aux = 0
     tmp = CREATE_STRUCT(tmp, 'aux', aux)

     FOR idata=0, N_TAGS(obs.data)-1 DO BEGIN
        i = WHERE(obs.data.(idata).group GT 0)
        tmp.x = obs.data.(idata).x[i]
        tmp.y = obs.data.(idata).y[i]
        tmp.e = obs.data.(idata).e[i]
     ENDFOR

     obs = {expr: obs.expr, aux: obs.aux, data: obs.data, eval: tmp, plot: obs.plot}
     gfit_replace_obs, iobs, obs
  ENDFOR
END

