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
;GFIT MODEL COMPONENT
;
;NAME:
;  qsfit_comp_ironuv
;
;COMPONENT DESCRIPTION:
;  The iron template at UV wavelengths from Vestergaard and Wilkes,
;  2001.
;
;PARAMETERS:
;  NORM (units: [X*Y])
;    Total flux in the whole iron complex.
;
;  FWHM (units: km s^-1)
;    FWHM of emission lines. This parameter is used to broaden the
;    iron template by convolution with a Gaussian kernel.
;
;OPTIONS:
;  NONE
;
;REFERENCES:
;  Vestergaard and Wilkes, 2001, ApJS, 134, 1V
;  http://adsabs.harvard.edu/abs/2001ApJS..134....1V
;
FUNCTION qsfit_comp_ironuv_prepare, ref_x, ref_y
  COMPILE_OPT IDL2
  ON_ERROR, !glib.on_error

  gprint, 'Preparation of Vestergaard and Wilkes (2001) UV iron template...'

  ;;Check input files are available
  path = FILE_DIRNAME(ROUTINE_FILEPATH('qsfit_comp_ironuv_prepare', /is_function)) + PATH_SEP()
  path += 'VW2001' + PATH_SEP()
  IF (~gfexists(path + 'Fe_UVtemplt_A.asc')) THEN $
     MESSAGE, 'Could not found Vestergaard&Wilkes (2001) template files in directory: ' + path

  ;;Read input files
  template={x:0., y:0.}
  ia   = greadtexttable(path + 'Fe_UVtemplt_A.asc' , ' ', /drop, template=template)
  ib   = greadtexttable(path + 'Fe_UVtemplt_B.asc' , ' ', /drop, template=template)
  i34  = greadtexttable(path + 'Fe3UV34modelB2.asc', ' ', /drop, template=template)
  i47  = greadtexttable(path + 'Fe3_UV47.asc'      , ' ', /drop, template=template)
  i191 = greadtexttable(path + 'Fe2_UV191.asc'     , ' ', /drop, template=template)

  IF (0) THEN BEGIN
     PRINT, gminmax(ib.x)
     PRINT, gminmax(ib.x-ib.x)
     PRINT, gminmax(ib.x-i34.x)
     PRINT, gminmax(ib.x-i47.x)
     PRINT, gminmax(ib.x-i191.x)
     gdist, /print, ib.x-SHIFT(ib.x,1) ;;0.482

     ggp_data, ib.x, ib.y  , plot='w l t "b"'   , /clear
     ggp_data, ib.x, ia.y  , plot='w l t "a"'
     ggp_data, ib.x, i34.y , plot='w l t "i34"'
     ggp_data, ib.x, i47.y , plot='w l t "i47"'
     ggp_data, ib.x, i191.y, plot='w l t "i191"'
     ggp
  ENDIF

  ;;i47 and i191 have slightly different values for X, hence I
  ;;interpolate them
  i47.y  = INTERPOL(i47.y , i47.x , ib.x)  &  i47.x  = ib.x
  i191.y = INTERPOL(i191.y, i191.x, ib.x)  &  i191.x = ib.x

  ;;Prepare the normalized reference template: consider the B template
  ;;and the FeII UV191 and FeIII UV47 multiplets.
  ref_x = ib.x
  ref_y = ib.y + i47.y + i191.y
  ref_y /= INT_TABULATED(ref_x, ref_y)

  ;;Enlarge wavelength range to accomodate broadened template
  tmp = (FINDGEN(100)+1) * 0.482
  ref_x = [MIN(ref_x)-REVERSE(tmp), ref_x]
  ref_y = [                  tmp*0, ref_y]

  tmp = (FINDGEN(700)+1) * 0.482
  ref_x = [ref_x, MAX(ref_x)+tmp]
  ref_y = [ref_y,          tmp*0]
  
  ;;Pre-compute the broadened teplates
  ;;(see Sect. 4.1 of Vestergaard&Wilkes 2001)

  ;;The width of the reference template is assumed to be that of I Zw1
  ref_fwhm = 900. ;;[km s^-1]

  ;;Grid of FWHM values
  fwhm = gloggen(1.e3, 2.e4, 300)


  ;;Prepare an evenly spaced logarithmic wavelength grid
  log_x = ggen(ALOG10(gminmax(ref_x)), 1e4)

  ;;Prepare return structure
  templ = {  x:   ref_x,                        $
             y:   FLTARR(gn(ref_x), gn(fwhm)),  $
             fwhm: fwhm                         $
          }

  ;;Interpolate template on this grid
  log_y = INTERPOL(ref_y, ref_x, 10.d^log_x)
  FOR i=0, gn(fwhm)-1 DO BEGIN
     ;;Compute sigma of Gaussian profile in units of km s^-1
     sigma = SQRT(fwhm[i]^2. - ref_fwhm^2.) / 2.35

     ;;Normalize by c
     sigma /= 3.e5

     ;;Prepare the convolution kernel: a Gaussian profile
     gauss = ggauss(log_x, MEAN(log_x), sigma)
     gauss = gauss[WHERE(gauss GT MAX(gauss)/1.e3)]

     ;;Convolve template and switch back to linear wavelength space
     templ.y[*,i] = INTERPOL( $
                    CONVOL(log_y, gauss, /edge_zero, /norm), $
                    10.d^log_x, ref_x)

     ;;Ensure template is normalized
     templ.y[*,i] /= INT_TABULATED(templ.x, templ.y[*,i])
  ENDFOR
  
  IF (0) THEN BEGIN
     ggp_clear
     ggp_cmd, xtit='Wavelength [AA]', ytit='Flux density [arb. units]'
     ggp_data, ref_x, ref_y, plot='w l t "FWHM=' + gn2s(ref_fwhm) + ' km/s (ref)"'
     dummy = MIN(ABS(templ.fwhm - 3000), i3000)
     FOREACH i, [0, i3000, gn(templ.fwhm)-1] DO BEGIN
        ggp_data, templ.x, templ.y[*,i], plot='w l t "FWHM=' + gn2s(templ.fwhm[i]) + ' km/s"'
        
        x  = templ.x
        y0 = templ.y[*,i]
        xref = 2350.;INT_TABULATED(x, x*y0)
        
        y1 = (x/xref)^(0.5)
        y2 = (x/xref)^(-1.7)
        y3 = (x/xref)^(-3.)

        PRINT, templ.fwhm[i], xref, INT_TABULATED(x, y0/y1), INT_TABULATED(x, y0/y2), INT_TABULATED(x, y0/y3)
     END
     ggp
  ENDIF

  RETURN, templ
END


PRO qsfit_comp_ironuv_init, comp
  COMPILE_OPT IDL2
  ON_ERROR, !glib.on_error
  COMMON COM_qsfit_comp_ironuv, templ

  IF (gn(templ) EQ 0) THEN BEGIN
     path = FILE_DIRNAME(ROUTINE_FILEPATH('qsfit_comp_ironuv_prepare', /IS_FUNCTION)) + PATH_SEP()
     file = path + 'qsfit_comp_ironuv.dat'
     IF (gfexists(file)) THEN $
        RESTORE, file $
     ELSE BEGIN
        templ = qsfit_comp_ironuv_prepare()
        SAVE, file=file, /compress, templ
     ENDELSE
  ENDIF

  comp.par.ew.val = 100
  comp.par.ew.limits[0] = 0

  comp.par.fwhm.val    = 3000
  comp.par.fwhm.limits = gminmax(templ.fwhm)
  comp.par.fwhm.step   = 200

  comp.par.cont2350.val   = 1
  comp.par.cont2350.fixed = 1
  comp.par.cont2350.tied  = 'qsfit_comp_sbpowerlaw_l2350()'
END


FUNCTION qsfit_comp_ironuv_opt, comp
  opt = {fwhmFixed: gnan()}
  RETURN, opt
END


FUNCTION qsfit_comp_ironuv_cdata, comp, x, cdata
  COMPILE_OPT IDL2
  ON_ERROR, !glib.on_error
  COMMON COM_qsfit_comp_ironuv, templ
  IF (gn(cdata) GT 0) THEN RETURN, cdata

  ifwhm = LINDGEN(gn(templ.fwhm))
  IF (FINITE(comp.opt.fwhmFixed)) THEN $
     dummy = MIN(ABS(comp.opt.fwhmFixed - templ.fwhm), ifwhm)

  ;;Initialize templates using current X values
  gprint, 'Interpolation of Vestergaard and Wilkes (2001) UV iron template...'
  yy = FLTARR(gn(x), gn(ifwhm))
  FOR i=0, gn(ifwhm)-1 DO $
     yy[*, i] = INTERPOL(REFORM(templ.y[*,ifwhm[i]]), templ.x, x)

  ;;For high values of FWHM the template may not go to zero and the
  ;;interpolation may produce negative value.  Ensure we have positive
  ;;values.
  yy = (yy > 0)

  cdata = {fwhm: templ.fwhm[ifwhm], y: yy}

  RETURN, PTR_NEW(cdata)
END

FUNCTION qsfit_comp_ironuv, x, ew, fwhm, cont2350, cdata=cdata, opt=opt
  COMPILE_OPT IDL2
  ON_ERROR, !glib.on_error

  IF (FINITE(opt.fwhmFixed)) THEN BEGIN
     IF (opt.fwhmFixed NE fwhm) THEN $
        MESSAGE, 'FWHM value should be fixed to ' + gn2s(opt.fwhmFixed), ' while it is ' + gn2s(fwhm)
  ENDIF

  ;;Search for the template with the closest value of FWHM
  dummy = MIN(ABS(fwhm - (*cdata).fwhm), i)

  ret = ew * cont2350 * REFORM((*cdata).y[*, i])
  RETURN, ret
END
