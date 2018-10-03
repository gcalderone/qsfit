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
  cachePar = []
  FOR iobs=0, N_TAGS(gfit.obs)-1 DO BEGIN
     obs = gfit.obs.(iobs)
     nn = 0l
     FOR idata=0, N_TAGS(obs.data)-1 DO $
        nn += gn(WHERE(obs.data.(idata).group GT 0, /null))

     tmp = { x: FLTARR(nn),  $
             y: FLTARR(nn),  $
             e: FLTARR(nn),  $
             m: FLTARR(nn),  $
             id: BYTARR(nn)  $
           }
     tmp.m = gnan()
     IF (N_TAGS(gfit.comp) GT 0) THEN BEGIN
        cnames = TAG_NAMES(gfit.comp)
        FOR icomp=0, N_TAGS(gfit.comp)-1 DO BEGIN
           val = REPLICATE(gnan(), nn)
           IF (~gfit.comp.(icomp).enabled) THEN $
              val = gfit.comp.(icomp).disabled_val
           tmp = CREATE_STRUCT(tmp, cnames[icomp], FLOAT(val))
        ENDFOR
     ENDIF

     aux = {}
     val = REPLICATE(gnan(), nn)
     FOR iaux=0, N_TAGS(obs.aux)-1 DO $
        aux = CREATE_STRUCT(aux, (TAG_NAMES(obs.aux))[iaux], val)
     IF (N_TAGS(aux) EQ 0) THEN $
        aux = 0
     tmp = CREATE_STRUCT(tmp, 'aux', aux)

     nn = 0l
     FOR idata=0, N_TAGS(obs.data)-1 DO BEGIN
        IF (gsearch(obs.data.(idata).group GT 0, i)) THEN BEGIN
           tmp.x[nn:nn+gn(i)-1] = obs.data.(idata).x[i]
           tmp.y[nn:nn+gn(i)-1] = obs.data.(idata).y[i]
           tmp.e[nn:nn+gn(i)-1] = obs.data.(idata).e[i]
           tmp.id[nn:nn+gn(i)-1] = idata
           nn += gn(i)
        ENDIF
     ENDFOR

     i = SORT(tmp.x)
     tmp.x = tmp.x[i]
     tmp.y = tmp.y[i]
     tmp.e = tmp.e[i]
     tmp.id = tmp.id[i]

     obs = {expr: obs.expr, aux: obs.aux, data: obs.data, eval: tmp, plot: obs.plot}
     gfit_replace_obs, iobs, obs
  ENDFOR
END

