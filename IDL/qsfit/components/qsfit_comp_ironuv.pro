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
FUNCTION qsfit_comp_ironuv_prepare
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

  ;;Split at these wavelengths
  split = [2140, 2654, 2800]
  gprint, 'Split template at wavelengths: ' + STRJOIN(gn2s(split), ', ') + ' AA'

  ;;Prepare return structure
  templ = {  x:   ref_x,                        $
             y:   FLTARR(gn(ref_x), gn(fwhm)),  $
             fwhm: fwhm                         $
          }
  templ = REPLICATE(templ, gn(split)+1)


  split = [MIN(10.d^log_x), split, MAX(10.d^log_x)]
  FOR iSplit=0, gn(split)-2 DO BEGIN
     ;;Interpolate template on this grid
     log_y = INTERPOL(ref_y, ref_x, 10.d^log_x)

     ii = WHERE(10.d^log_x LT split[iSplit]  OR  10.d^log_x GT split[iSplit+1])
     gassert, ii[0] NE -1
     log_y[ii] = 0

     FOR i=0, gn(fwhm)-1 DO BEGIN
        ;;Compute sigma of Gaussian profile in units of km s^-1
        sigma = SQRT(fwhm[i]^2. - ref_fwhm^2.) / 2.35

        ;;Normalize by c
        sigma /= 3.e5

        ;;Prepare the convolution kernel: a Gaussian profile
        gauss = ggauss(log_x, MEAN(log_x), sigma)
        gauss = gauss[WHERE(gauss GT MAX(gauss)/1.e3)]

        ;;Convolve template and switch back to linear wavelength space
        templ[iSplit].y[*,i] = INTERPOL( $
                               CONVOL(log_y, gauss, /edge_zero, /norm), $
                               10.d^log_x, ref_x)

        ;;Ensure template is normalized
        templ[iSplit].y[*,i] /= INT_TABULATED(templ[iSplit].x, templ[iSplit].y[*,i])
     ENDFOR
  ENDFOR
  
  IF (0) THEN BEGIN
     FOR iSplit=0, gn(split)-2 DO BEGIN
        ggp_clear
        ggp_cmd, xtit='Wavelength [AA]', ytit='Flux density [arb. units]'
        ggp_data, ref_x, ref_y, plot='w l t "FWHM=' + gn2s(ref_fwhm) + ' km/s (ref)"'
        dummy = MIN(ABS(templ[iSplit].fwhm - 3000), i3000)
        FOREACH i, [0, i3000, gn(templ[iSplit].fwhm)-1] DO $
           ggp_data, templ[iSplit].x, templ[iSplit].y[*,i], plot='w l t "FWHM=' + gn2s(templ[iSplit].fwhm[i]) + ' km/s"'
        ggp
        gkey
     ENDFOR
  ENDIF

  RETURN, templ
END


PRO qsfit_comp_ironuv_init, comp
  COMPILE_OPT IDL2
  ON_ERROR, !glib.on_error
  COMMON COM_qsfit_comp_ironuv, templ, cur

  IF (gn(templ) EQ 0) THEN BEGIN
     path = FILE_DIRNAME(ROUTINE_FILEPATH('qsfit_comp_ironuv_init')) + PATH_SEP()
     file = path + 'qsfit_comp_ironuv.dat'
     IF (gfexists(file)) THEN $
        RESTORE, file $
     ELSE BEGIN
        templ = qsfit_comp_ironuv_prepare()
        SAVE, file=file, /compress, templ
     ENDELSE
  ENDIF
  cur = []

  comp.norm.val = 0.01
  comp.norm.limits[0] = 0

  comp.fwhm.val    = 3000
  comp.fwhm.limits = gminmax(templ.fwhm)
  comp.fwhm.step   = 200
END

FUNCTION qsfit_comp_ironuv_part
  RETURN, 2
END


FUNCTION qsfit_comp_ironuv, x, norm, fwhm, part=part
  COMPILE_OPT IDL2
  ON_ERROR, !glib.on_error
  COMMON COM_qsfit_comp_ironuv

  ;;Initialize templates using current X values
  IF (gn(cur) EQ 0) THEN BEGIN
     gprint, 'Interpolation of Vestergaard and Wilkes (2001) UV iron template...'
     cur = FLTARR(gn(x), gn(templ[part].fwhm), gn(templ))
     FOR iPart=0, gn(templ)-1 DO BEGIN
        FOR i=0, gn(templ[iPart].fwhm)-1 DO BEGIN
           cur[*, i, iPart] = INTERPOL(REFORM(templ[iPart].y[*,i]), templ[iPart].x, x)
        ENDFOR
     ENDFOR

     ;;For high values of FWHM the template may not go to zero and the
     ;;interpolation may produce negative value.  Ensure we have
     ;;positive values.
     cur = (cur > 0)
  ENDIF

  ;;Search for the template with the closest value of FWHM
  dummy = MIN(ABS(fwhm - templ[part].fwhm), i)
  ret = norm * REFORM(cur[*, i, part])
  RETURN, ret
END
