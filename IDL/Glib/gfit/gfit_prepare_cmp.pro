;=====================================================================
;NAME:
;  gfit_prepare_cmp
;
;PURPOSE:
;  Prepare the GFIT.CMP structure according to currently selected
;  data.
;
;PARAMETERS:
;  NONE
;
PRO gfit_prepare_cmp
  COMPILE_OPT IDL2
  ON_ERROR, !glib.on_error
  COMMON GFIT_PRIVATE
  COMMON GFIT

  IF (gfit.data.nn EQ 0) THEN $
     MESSAGE, 'No data available.'

  ;;Prepare the CMP structure
  template_cmp = $
     {x:      0.,  $
      nx:     1l,  $
      y:      0.,  $
      e:      0.,  $
      m:  gnan()   $
     }

  cmp = []
  dataNoticed = []
  FOR idata=0, gfit.data.nn-1 DO BEGIN
     c = REPLICATE(template_cmp, gn(gfit.data.(idata).y))

     IF (gtype(gfit.data.(idata).x[0], /float)) THEN $
        c.x = gfit.data.(idata).x $
     ELSE $
        c.x = FINDGEN(gn(gfit.data.(idata).y))

     c.nx    = 1
     c.y     = gfit.data.(idata).y
     c.e     = gfit.data.(idata).e
     c.m     = gnan()

     tmp = WHERE( gfit.data.(idata).group GE 0 )
     IF (tmp[0] EQ -1) THEN MESSAGE, 'All data are ignored for dataset ' + gn2s(idata)
     dataNoticed = CREATE_STRUCT(dataNoticed, 'D' + gn2s(idata), tmp)

     c = c[dataNoticed.(idata)]
     cmp = CREATE_STRUCT(cmp, 'D' + gn2s(idata), c)
  ENDFOR

  gfit = { $
           opt:   gfit.opt     $
         , data:  gfit.data    $
         , comp:  gfit.comp    $
         , expr:  gfit.expr    $
         , cmp:   cmp          $
         , res:   template_res $
         , plot:  gfit.plot    $
         }
END

