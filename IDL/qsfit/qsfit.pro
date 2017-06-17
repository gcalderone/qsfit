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
;  qsfit_version
;
;PURPOSE:
;  Return a string which identifies the QSFIT version.
;
;RETURN VALUE: (a scalar string)
;  The QSFIT version.
;
FUNCTION qsfit_version
  RETURN, '1.1'
END


;=====================================================================
;NAME:
;  qsfit_log
;
;PURPOSE:
;  Collect and print log messages.
;
;DESCRIPTION:
;  The messages are immediately printed, and collected to be later
;  re-printed (when the /print keyword is given).  All printing occur
;  through the gprint routine.
;
;PARAMETERS:
;  MSG  (input, a scalar or array of strings)
;    The string to be printed and collected.
;
;  OUT= (output, array of strings)
;    Return all messages collected so far.
;
PRO qsfit_log, msg, out=out
  COMPILE_OPT IDL2
  ON_ERROR, !glib.on_error
  COMMON COM_QSFIT_LOG, collected

  IF (gn(collected) EQ 0) THEN collected = LIST()

  IF (ARG_PRESENT(out)) THEN BEGIN
     IF (N_ELEMENTS(collected) EQ 0) THEN RETURN
     ;;Return all messages collected so far and clear the stack
     out = collected.toArray()
     collected = LIST()
     RETURN
  ENDIF


  IF (N_PARAMS() EQ 0) THEN msg = ''

  FOR i=0, gn(msg)-1 DO $
     collected.add, STRING(msg[i])
  gprint, msg
END



;=====================================================================
;NAME:
;  qsfit_spec2restframe
;
;PURPOSE:
;  Convert observed flux density into rest frame emitted luminosity
;  density.
;
;PARAMETERS:
;  X (input/output, an array of numbers)
;    The observed wavelengths (in Angstrom).  Upon exit it contains
;    the rest frame wavelengths.
;
;  Y (input/output, an array of numbers)
;    The observed flux density (in units of 10^-17 erg s^-1 cm^-2
;    A^-1).  Upon exit it contains the emitted luminosity density
;    (assuming isotropic emission, in units of (10^42 erg s^-1 A^-1).
;
;  E (input/output, an array of numbers)
;    The uncertainties associated to Y values (same units as Y
;    values).
;
;  Z (input, a scalr number)
;    The source redshift
;
;  EBV (input, a scalar number)
;    The source colour excess
;
PRO qsfit_spec2restframe, x, y, e, z, ebv
  COMPILE_OPT IDL2
  ON_ERROR, !glib.on_error

  qsfit_log
  qsfit_log, 'E(B-V) : ' + gn2s(ebv)

  ;;De-reddening
  CCM_UNRED, [1450, 3000, 5100.], [1,1,1.], DOUBLE(ebv), dered
  qsfit_log, 'Dereddening factors @ 1450, 3000, 5100 AA: ' + STRJOIN(gn2s(dered), ', ')
  CCM_UNRED, x, y, ebv, y
  CCM_UNRED, x, e, ebv, e
  gprint

  ;;Transform to rest-frame
  x /= (1.d + z)
  y *= (1.d + z)
  e *= (1.d + z)

  ;;Cosmology
  cosmo = {H0: 70., Omega_m:0.3, Lambda0:0.7 } ;;S11
  qsfit_log, 'Cosmology: '
  gps, cosmo, out=tmp
  qsfit_log, tmp

  ;Luminosity distance in Gpc
  dl = LUMDIST(z, _EXTRA=cosmo, /silent) / 1000.d
  qsfit_log, 'Redshift: ' + gn2s(z)
  qsfit_log, 'Lum. distance (Gpc): ' + gn2s(dl)
  qsfit_log

  ;4 * !PI * 1e-17 * (1.e9 * parsec)^2 / 10.d^42 = 1.1967452e-03
  flux2lum = dl^2.d * 1.1967452e-03

  y *= flux2lum
  e *= flux2lum
END




;=====================================================================
;NAME:
;  qsfit_prepare
;
;PURPOSE:
;  Read a SDSS DR-10 spectrum and loads data into GFIT.
;
;PARAMETERS:
;  FILENAME (input, a scalar string)
;    The path to a SDSS DR-10 spectrum file.
;
;  ID= (optional input, a scalar string)
;    A spectrum identifier to be inserted into GFIT dataset user
;    data.
;
;  Z= (optional input, a scalar number)
;   The source redshift.  If not given the redshift is read from the
;   FITS file.
;
;  EBV= (optional input, a scalar number)
;   The source colour excess.  If not given it is calculated using the
;   Schlegel et al. (1998) maps.
;
PRO qsfit_prepare, filename, ID=id, Z=z, EBV=ebv
  COMPILE_OPT IDL2
  ON_ERROR, !glib.on_error
  COMMON GFIT

  ;;Read SDSS10 spSpec file
  IF (~gfexists(filename)) THEN $
     MESSAGE, 'File: ' + filename + ' does not exists'

  fits = MRDFITS(filename, 0, head, /silen)

  ;;Check the file is actually a DR10 spectrum
  IF (gn(fits) EQ 1) THEN BEGIN
     fits = MRDFITS(filename, 1, /silen)
  ENDIF $
  ELSE BEGIN
     MESSAGE, 'FITS file is not a DR10 file'
  ENDELSE

  ;;Redshift
  IF (~KEYWORD_SET(z)) THEN BEGIN
     z = MRDFITS(filename, 2, /silen)
     z = z.z
  ENDIF
  IF (z LE 0) THEN $
     MESSAGE, 'Redhistf is negative: ' + gn2s(z)

  IF (z GT 2.12) THEN $
     MESSAGE, 'Analysis of sources with Z>2.12 is not yet supported'

  ;;Coordinates
  IF (gsearch(STRTRIM(STRMID(head, 0, 8), 2) EQ 'PLUG_RA' , i)) THEN ra = head[i]
  IF (gsearch(STRTRIM(STRMID(head, 0, 8), 2) EQ 'PLUG_DEC', i)) THEN de = head[i]
  ra = gfloat(STRMID(ra, 9))
  de = gfloat(STRMID(de, 9))
  qsfit_log, 'RA :  ' + gn2s(ra)
  qsfit_log, 'DEC:  ' + gn2s(de)
  qsfit_log, 'Redshift: ' + gn2s(z)

  IF (~KEYWORD_SET(ebv)) THEN BEGIN
     ;;Evaluate E(B-V) using dust maps from Schlegel 1998.  The
     ;;coordinates (RA, DEC, J2000) are required.
     EULER, ra, de, glon, glat, 1
     ebv = CALL_FUNCTION('DUST_GETVAL', glon, glat, /interp, map='Ebv')
  ENDIF

  ;;Borders may be noisy, drop a few channels on both side
  ndrop = 100
  fits = fits[ndrop:gn(fits)-ndrop-1]

  ;;Drop data below 1210 AA since it is significantly absorbed
  i = WHERE(10.d^fits.loglam GT 1210)
  gassert, i[0] NE -1
  fits = fits[i]

  good = (fits.and_mask EQ 0)   AND   $
         (fits.ivar GT 0)       AND   $
         (fits.flux GT 0)

  ;;Identify "good" channels
  iGood = WHERE(good)
  IF (iGood[0] EQ -1) THEN $
     MESSAGE, 'No "good" spectrum channel'

  goodFraction = FLOAT(gn(iGood)) / gn(fits)
  IF (goodFraction LT 0.75) THEN $
     MESSAGE, 'Only ' + gn2s(goodFraction*100.) + '% spectrum channels have "good" mask flag'
  qsfit_log, 'Fraction of "good" channels: ' +  gn2s(goodFraction*100.) + '%'
  qsfit_log

  ;;Set ivar of "bad" channels to NaN (to avoid "divide by 0" error)
  tmp = REPLICATE(gnan(), gn(fits.ivar))
  tmp[iGood] = fits[iGood].ivar
  fits.ivar = tmp


  ;;Prepare user data with additional info from the FITS file.
  IF (gn(id) EQ 0) THEN id = ''

  xx = 10.d^fits.loglam
  udata = {id:   STRING(id), $
           file: (STRSPLIT(filename, PATH_SEP(), /extract))[-1], $
           ebv:  ebv       , $
           z:    z         , $
           ra:   ra        , $
           dec:  de        , $
           fitshead: head  , $
           median_flux: 0. , $
           median_err:  0. , $
           goodFraction: goodFraction $
          }

  tmp = xx - SHIFT(xx,1)
  tmp = tmp[1:*] / xx[1:*]
  qsfit_log, 'Spectral resolution (min, max): ' + STRJOIN(gn2s(gminmax(tmp*3.e5)), ", ") + ' km s^-1'

  ;;Get X, Y and error quantities
  xx = 10.d^fits.loglam
  yy = DOUBLE(fits.flux)
  ee = SQRT(1.d / fits.ivar)

  ;;Transform to rest frame.  Final units are:
  ;;xx     : AA
  ;;yy, ee : 10^42 erg s^-1 AA^-1
  qsfit_spec2restframe, xx, yy, ee, z, ebv

  ;;Save median flux and error
  udata.median_flux = MEDIAN(yy[iGood])
  udata.median_err  = MEDIAN(ee[iGood])

  ;;Initialize gfit
  gfit_init

  ;;Add data into gfit
  gfit_add_data, xx, yy, ee, UDATA=udata

  ;;Ignore "bad" channels
  gfit.data.(0).group = -1
  gfit.data.(0).group[iGood] = 1
  ;;gfit.data.(0).group[WHERE(xx LT 4000)] = -1
  gfit_prepare_cmp

  ;;Ignore data below emission lines with insufficient coverage.
  ;;Note: this steps must be performed before adding components since
  ;;some of them rely on the assumption that the X values do not vary
  ;;between one call and the other.
  qsfit_ignore_data_on_missing_lines

  ;;Setup appropriate titles for plot
  tmp = (STRSPLIT(filename, '/', /extract))[-1]  ;;extract filename, drop directories
  gfit.plot.(0).main.title = tmp + ', z=' + gn2s(z) + ', E(B-V)=' + gn2s(ebv)
  gfit.plot.(0).main.rebin = 1
  gfit.plot.(0).data.label = 'Data'

  ;;angstrom = '!6!sA!r!u!9 %!6!n'
  gfit.plot.(0).main.xtit  = 'Rest frame wavelength [A]'
  gfit.plot.(0).main.ytit  = 'Lum. density [10^{42} erg s^{-1} A^{-1}]'
END



;=====================================================================
;NAME:
;  qsfit_line_coverage
;
;PURPOSE:
;  Returns the line coverage fraction of an emission line.
;
;DESCRIPTION:
;  The line coverage fraction is the ratio of "good" channels over all
;  available channels for an emission line with given width.  The
;  minimum line coverage is 0, the maximum is 1.  A good line
;  coverage, say above 0.5, ensure that the line quantities are well
;  constrained (provided the line is sufficiently bright).  If the
;  whole line width lies outside the available wavelength range the
;  return value is 0.
;
;PARAMETERS:
;  WAVE (input, a scalar number)
;    The wavelength of the line to be considered, in Angstrom.
;
;  WIDTH (input, a scalar number)
;    The range (centered on WAVE) where the line coverage is to be
;    evaluated (in km s^-1).
;
;  GOOD= (output, an array of integers).
;    The indices of "good" spectral channels in the selected range.
;
;  TOTAL= (output, an array of integers).
;    The indices of all spectral channels in the selected range.
;
;RETURN VALUE: (a scalar floating point)
;  The emission line coverage
;
FUNCTION qsfit_line_coverage, wave, width, TOTAL=total, GOOD=good
  COMPILE_OPT IDL2
  ON_ERROR, !glib.on_error
  COMMON GFIT

  width_aa = width / 3.e5 * wave

  ;;Evaluate GOOD and TOTAL indices
  good  = WHERE(ABS(gfit.data.(0).x - wave) LT width_aa/2.   AND   (gfit.data.(0).group GE 0))
  total = WHERE(ABS(gfit.data.(0).x - wave) LT width_aa/2.)

  IF (total[0] EQ -1) THEN RETURN, 0
  IF (good [0] EQ -1) THEN RETURN, 0

  ;;Ensure the whole line is visible within the spectrum
  IF (gfit.data.(0).x[0]  GT wave - width_aa/2.) THEN RETURN, 0.
  IF (gfit.data.(0).x[-1] LT wave + width_aa/2.) THEN RETURN, 0.

  coverage = FLOAT(gn(good)) / FLOAT(gn(total))
  RETURN, coverage
END



;=====================================================================
;NAME:
;  qsfit_freeze
;
;PURPOSE:
;  Freeze or thaw component parameters.
;
;PARAMETERS:
;  CONT= (optional input, either 0 or 1)
;    Freeze (1) or thaw (0) all parameters related to the continuum
;    and the offset components.  The continuum curvature is always
;    kept fixed.
;
;  IRON= (optional input, either 0 or 1)
;    Freeze (1) or thaw (0) all parameters related to both the UV and
;    optical iron components.
;
;  LINES= (optional input, either 0 or 1)
;    Freeze (1) or thaw (0) all parameters related to the emission
;    lines.
;
PRO qsfit_freeze, cont=cont, iron=iron, lines=lines
  COMPILE_OPT IDL2
  ON_ERROR, !glib.on_error
  COMMON GFIT

  IF (N_ELEMENTS(cont) EQ 1) THEN BEGIN
     gfit.comp.continuum.norm.fixed    = cont
     gfit.comp.continuum.x0.fixed      = 1
     gfit.comp.continuum.alpha1.fixed  = cont
     gfit.comp.continuum.dalpha.fixed  = cont
     gfit.comp.continuum.curv.fixed    = 1
  ENDIF

  IF (N_ELEMENTS(iron) EQ 1) THEN BEGIN
     gfit.comp.ironuv0.norm.fixed    = iron
     gfit.comp.ironuv0.fwhm.fixed    = 1
     gfit.comp.ironuv1.norm.fixed    = iron
     gfit.comp.ironuv1.fwhm.fixed    = 1
     gfit.comp.ironuv2.norm.fixed    = iron
     gfit.comp.ironuv2.fwhm.fixed    = 1
     gfit.comp.ironuv3.norm.fixed    = iron
     gfit.comp.ironuv3.fwhm.fixed    = 1
     gfit.comp.ironopt.norm_br.fixed = iron
     gfit.comp.ironopt.fwhm_br.fixed = 1
     gfit.comp.ironopt.norm_na.fixed = iron
     gfit.comp.ironopt.fwhm_na.fixed = 1
  ENDIF

  IF (N_ELEMENTS(lines) EQ 1) THEN BEGIN
     FOR i=0, gfit.comp.nn-1 DO BEGIN
        IF (gfit.comp.(i).funcName EQ 'qsfit_comp_emline') THEN BEGIN
           IF ((gfit.comp.(i).enabled)) THEN BEGIN

              gfit.comp.(i).norm.fixed = lines
              gfit.comp.(i).fwhm.fixed = lines

              isUnk = (STRMID(STRUPCASE(gfit.comp.(i).name), 0, 3) EQ 'UNK')
              IF (isUnk) THEN gfit.comp.(i).center.fixed = lines  $
              ELSE            gfit.comp.(i).v_off.fixed  = lines
           ENDIF
        ENDIF
     ENDFOR

     ;; Fix emission line parameter values (CUSTOMIZABLE)
     ;;gfit.comp.na_Hb.norm.val    = 0.17507
     ;;gfit.comp.na_Hb.fwhm.val    = 505.78
     ;;gfit.comp.na_Hb.v_off.val   = -130.34
     ;;gfit.comp.na_Hb.norm.fixed  = 1
     ;;gfit.comp.na_Hb.fwhm.fixed  = 1
     ;;gfit.comp.na_Hb.v_off.fixed = 1
     ;;
     ;;gfit.comp.na_OIII_5007.norm.val    = 0.46382
     ;;gfit.comp.na_OIII_5007.fwhm.val    = 604.87
     ;;gfit.comp.na_OIII_5007.v_off.val   = -2.0543
     ;;gfit.comp.na_OIII_5007.norm.fixed  = 1
     ;;gfit.comp.na_OIII_5007.fwhm.fixed  = 1
     ;;gfit.comp.na_OIII_5007.v_off.fixed = 1
  ENDIF
END




;=====================================================================
;NAME:
;  qsfit_compile
;
;PURPOSE:
;  Prepare model expression (actually the sum of all components) and
;  compile GFIT model.
;
;PARAMETERS:
;  NONE.
;
PRO qsfit_compile
  COMPILE_OPT IDL2
  ON_ERROR, !glib.on_error
  COMMON GFIT

  ;;Setup GFIT model expression: actually the sum of all components
  cnames = (TAG_NAMES(gfit.comp))[0:gfit.comp.nn-1] ;;Model
  gfit.expr.(0).model = STRJOIN(cnames, ' + ')

  ;;Compile GFIT model
  gfit_compile
END



;=====================================================================
;NAME:
;  qsfit_add_continuum
;
;PURPOSE:
;  Add continuum components (a smoothly broken powerlaw and the host
;  galaxy template) to the GFIT model, and add the expressions to plot
;  both the continuum emission and the host galaxy contribution.
;
;PARAMETERS:
;  NONE.
;
;NOTES:
;  The galaxy template is the elliptical template from Mannucci et
;  al. 2001, MNRAS, 326, 745M.  This component i disabled if the
;  source redshift is above 0.8.
;
PRO qsfit_add_continuum
  COMPILE_OPT IDL2
  ON_ERROR, !glib.on_error
  COMMON GFIT

  gprint, 'Adding continuum component(s)...'

  ;;--------------------------
  gprint, '   Continuum'
  continuum = gfit_component('qsfit_comp_sbpowerlaw')

  continuum.norm.val = MEAN(gfit.cmp.(0).y) / 2.
  continuum.norm.limits = [1.e-10, gnan()]

  ;;Limit the value for x0 to ensure the change of slope occurs within
  ;;the wavelength range.
  mm = gminmax(gfit.cmp.(0).x)
  continuum.x0.val    = mm[0] + (mm[1]-mm[0]) / 2.
  continuum.x0.limits = mm[0] + (mm[1]-mm[0]) / 5. * [1, 4]
  continuum.x0.fixed  = 0

  continuum.alpha1.val = -1.5
  continuum.alpha1.limits = [-3, 1.] ;--> -3, 1 in frequency

  continuum.dalpha.val    = 0
  continuum.dalpha.limits = [-0.2, 0.2]
  ;;continuum.dalpha.fixed  = 1 ;;CUSTOMIZABLE: set fixed=1 to use a simple power law component

  ;;Keep a fixed curvature of the smoothly broken power law
  continuum.curv.val = 100
  continuum.curv.fixed = 1

  gfit_add_comp, type=continuum, 'Continuum'

  gfit_add_expr, 'expr_Continuum', 'continuum'
  gfit.plot.(0).expr_continuum.label = 'Continuum'
  gfit.plot.(0).expr_continuum.gp = 'w line ls 1 dt 2 lw 1 lt rgb "red"'

  ;;--------------------------
  gprint, '   Galaxy'
  gfit_add_comp, type='qsfit_comp_galaxytemplate', 'galaxy'
  gfit.comp.galaxy.opt.id = 'M01_E'
  ;;gfit.comp.galaxy.opt.id = 'SWIRE_ELL2'
  ;;gfit.comp.galaxy.opt.id = 'SWIRE_SC'
  ;;gfit.comp.galaxy.opt.id = 'SWIRE_ARP220'

  gfit.comp.galaxy.norm.val = INTERPOL(gfit.cmp.(0).y, gfit.cmp.(0).x, 5500) > 1.e-4

  ;;If 5500 is outisde the available range compute value at the edge
  ;;(solve problems for e.g., spec-0411-51817-0198)
  IF (MAX(gfit.data.(0).x) LT 5500) THEN $
     gfit.comp.galaxy.norm.val = INTERPOL(gfit.cmp.(0).y, gfit.cmp.(0).x, MAX(gfit.data.(0).x)) > 1.e-4

  gfit.comp.galaxy.norm.limits = [0, gnan()]
  gfit.comp.galaxy.norm.fixed = 0

  gfit_add_expr, 'expr_Galaxy', 'galaxy'
  gfit.plot.(0).expr_galaxy.label = 'Galaxy'
  gfit.plot.(0).expr_galaxy.gp = 'w line ls 1 dt 2 lw 1 lt rgb "dark-red"'
  gfit.plot.(0).expr_galaxy.plot = 0 ;;do not plot this component

  gfit_add_expr, 'expr_ContGalaxy', 'continuum + galaxy'
  gfit.plot.(0).expr_ContGalaxy.label = 'Cont. + Galaxy'
  gfit.plot.(0).expr_ContGalaxy.gp = 'w line ls 1 lt rgb "red"'

  ;;Galaxy component is disabled when max(wavelength) < 4480, corresponding to z~0.95
  IF (gfit.data.(0).udata.z GT 0.7    AND   (gfit.comp.galaxy.opt.id EQ 'M01_E')) THEN BEGIN
     qsfit_log, 'Galaxy component is disabled since Z=' + gn2s(gfit.data.(0).udata.z) + ' > 0.7'
     gfit.comp.galaxy.enabled = 0
  ENDIF

  qsfit_compile
END



;=====================================================================
;NAME:
;  qsfit_renormalize_cont
;
;PURPOSE:
;  Re-normalize the continuum to account for emission lines.
;
;DESCRIPTION:
;  The continuum components are fitted first, without considering the
;  emission lines, and the resulting residuals are (on average) 50%
;  positive and 50% negative.  In order to account for emission lines
;  contribution we lower the continuum luminosity until the negative
;  residuals are  90% of the total.
;
;PARAMETERS:
;  NONE.
;
PRO qsfit_renormalize_cont
  COMPILE_OPT IDL2
  ON_ERROR, !glib.on_error
  COMMON GFIT

  qsfit_log
  qsfit_log, 'Continuum renormalization (to account for emission lines)'
  last_fraction = -1.
  WHILE (1) DO BEGIN
     yy = gfit.cmp.(0).y
     ee = gfit.cmp.(0).e
     mm = gfit.cmp.(0).m

     residuals = (mm - yy) / ee
     check_fraction = gn(WHERE(residuals LT 0)) / FLOAT(gn(yy))


     IF (last_fraction EQ -1.) THEN $
        qsfit_log, 'Initial continuum norm. and fraction of negative residuals: ' + $
                  gn2s(gfit.comp.continuum.norm.val) + ',  ' + gn2s(check_fraction)

     IF (last_fraction EQ check_fraction) THEN BREAK
     last_fraction = check_fraction

     IF (check_fraction GT 0.9) THEN BREAK ;;CUSTOMIZABLE

     ;;Lower the continuum normalization
     gfit.comp.continuum.norm.val *= 0.99

     ;;Check we are still within the limits
     IF (gfit.comp.continuum.norm.val LE gfit.comp.continuum.norm.limits[0]) THEN BEGIN
        gfit.comp.continuum.norm.val = gfit.comp.continuum.norm.limits[0]
        gfit_eval
        BREAK
     ENDIF

     gfit_eval
  ENDWHILE
  qsfit_log, 'Final continuum norm. and fraction of negative residuals  : ' + $
             gn2s(gfit.comp.continuum.norm.val) + ',  ' + gn2s(check_fraction)
  qsfit_log
END


;=====================================================================
;NAME:
;  qsfit_nunklines
;
;PURPOSE:
;  Return the number of "unknown" lines to be considered.
;
;DESCRIPTION:
;  The "unknown" lines are emission lines whose center wavelength is
;  not a-priori assigned: they are placed (after all other emission
;  lines have been fitted) at wavelengths where the fitting residuals
;  are large.
;
;  By default we consider 10 such lines.
;
;PARAMETERS:
;  NONE.
;
;RETURN VALUE: (a scalar integer)
;  The number of "unknown" lines to be considered.
;
FUNCTION qsfit_nunklines
  RETURN, 10 ;;CUSTOMIZABLE
END




;=====================================================================
;NAME:
;  qsfit_lineset
;
;PURPOSE:
;  Return the list of emission lines to consider.
;
;PARAMETERS:
;  NONE
;
;RETURN VALUE: (an array of structure)
;  Each element in the array is a structure whose fields are:
;  NAME: name of emission line;
;  WAVE: reference wavelength for the emission line;;
;  TYPE: whether the line should be fitted with a broad component (B),
;   a narrow component (N) or both (BN);
;
FUNCTION qsfit_lineset
  str = {name:'', wave: 0., type: ''}

  all = LIST()

  ;;CUSTOMIZABLE, see:
  ;; - http://classic.sdss.org/dr6/algorithms/linestable.html
  ;; - https://ned.ipac.caltech.edu/level5/Netzer/Netzer2_1.html
  ;; - http://www.star.ucl.ac.uk/~msw/lines.html
  ;;str.name = 'OVI'         & str.wave = 1033.82   &  str.type = 'N'  & all.add, str
  ;;str.name = 'Lya'         & str.wave = 1215.24   &  str.type = 'BN' & all.add, str
  ;;str.name = 'NV'          & str.wave = 1240.81   &  str.type = 'B'  & all.add, str
  ;;str.name = 'OI'          & str.wave = 1305.53   &  str.type = 'B'  & all.add, str
  ;;str.name = 'CII'         & str.wave = 1335.31   &  str.type = 'B'  & all.add, str
    str.name = 'SiIV_1400'   & str.wave = 1399.8    &  str.type = 'B'  & all.add, str
    str.name = 'CIV_1549'    & str.wave = 1549.48   &  str.type = 'B'  & all.add, str
  ;;str.name = 'HeII'        & str.wave = 1640.4    &  str.type = 'B'  & all.add, str
  ;;str.name = 'OIII'        & str.wave = 1665.85   &  str.type = 'B'  & all.add, str
  ;;str.name = 'AlIII'       & str.wave = 1857.4    &  str.type = 'B'  & all.add, str
    str.name = 'CIII_1909'   & str.wave = 1908.734  &  str.type = 'B'  & all.add, str;;CIII]
  ;;str.name = 'CII'         & str.wave = 2326.0    &  str.type = 'B'  & all.add, str
    str.name = 'MgII_2798'   & str.wave = 2799.117  &  str.type = 'B'  & all.add, str
  ;;str.name = 'NeV'         & str.wave = 3346.79   &  str.type = 'N'  & all.add, str ;;[NeV]
    str.name = 'NeVI_3426'   & str.wave = 3426.85   &  str.type = 'N'  & all.add, str;;[NeVI]
    str.name = 'OII_3727'    & str.wave = 3729.875  &  str.type = 'N'  & all.add, str;;[OII]  ;was 3727.09
    str.name = 'NeIII_3869'  & str.wave = 3869.81   &  str.type = 'N'  & all.add, str;;[NeIII]
    str.name = 'Hd'          & str.wave = 4102.89   &  str.type = 'B'  & all.add, str
    str.name = 'Hg'          & str.wave = 4341.68   &  str.type = 'B'  & all.add, str
  ;;str.name = 'HeII'        & str.wave = ????      &  str.type = 'B'  & all.add, str
    str.name = 'Hb'          & str.wave = 4862.68   &  str.type = 'BN' & all.add, str
    str.name = 'OIII_4959'   & str.wave = 4960.295  &  str.type = 'N'  & all.add, str;;[OIII]
    str.name = 'OIII_5007'   & str.wave = 5008.240  &  str.type = 'N'  & all.add, str;;[OIII]
    str.name = 'HeI_5876'    & str.wave = 5877.30   &  str.type = 'B'  & all.add, str
    str.name = 'NII_6549'    & str.wave = 6549.86   &  str.type = 'N'  & all.add, str;;[NII]
    str.name = 'Ha'          & str.wave = 6564.61   &  str.type = 'BN' & all.add, str
    str.name = 'NII_6583'    & str.wave = 6585.27   &  str.type = 'N'  & all.add, str;;[NII]
    str.name = 'SII_6716'    & str.wave = 6718.29   &  str.type = 'N'  & all.add, str;;[SII]
    str.name = 'SII_6731'    & str.wave = 6732.67   &  str.type = 'N'  & all.add, str;;[SII]

  RETURN, all.toArray()
END




;=====================================================================
;NAME:
;  qsfit_ignore_data_on_missing_lines
;
;PURPOSE:
; Ignore the data where the wavelength coverage for an emission line
; is insufficient.
;
;PARAMETERS:
;  NONE
;
PRO qsfit_ignore_data_on_missing_lines
  COMPILE_OPT IDL2
  ON_ERROR, !glib.on_error
  COMMON GFIT

  ;;Get the list of lines to consider
  lines = qsfit_lineset()

  FOR i=0, gn(lines)-1 DO BEGIN
     ;;Estimate line coverage
     coverage = qsfit_line_coverage(lines[i].wave, (lines[i].type EQ 'N' ? 1e3 : 1.2e4), total=toBeIgnored)
     qsfit_log, 'The line ' + lines[i].name + ' has a coverage of ' + STRING(coverage)

     ;;Ensure that the whole line has at least 60% of "good" channels
     IF (coverage LT 0.6) THEN BEGIN
        IF (toBeIgnored[0] NE -1) THEN BEGIN
           qsfit_log, 'Ignoring data between ' + $
                      gn2s(ROUND(MIN(gfit.data.(0).x[tobeIgnored]))) + 'A and ' + $
                      gn2s(ROUND(MAX(gfit.data.(0).x[tobeIgnored]))) + 'A'
           gfit.data.(0).group[tobeIgnored]=-2
        ENDIF
     ENDIF
  ENDFOR
END



;=====================================================================
;NAME:
;  qsfit_add_lineset
;
;PURPOSE:
; Add the broad, narrow and "unknown" emission components to the GFIT
; model.
;
;PARAMETERS:
;  NONE.
;
PRO qsfit_add_lineset
  COMPILE_OPT IDL2
  ON_ERROR, !glib.on_error
  COMMON GFIT

  gprint, 'Adding emission lines...'

  ;;Prepare emission line component
  comp = gfit_component('qsfit_comp_emline')

  ;;Get the data and the current model to properly setup initial guess
  ;;for line normalizations.
  gfit_eval
  xx = gfit.cmp.(0).x
  yy = gfit.cmp.(0).y
  mo = gfit.cmp.(0).m

  ;;Get the list of lines to consider
  lines = qsfit_lineset()

  FOR i=0, gn(lines)-1 DO BEGIN
     ;;lines[i].type EQ 'N'  ==> Narrow line
     ;;lines[i].type EQ 'B'  ==> Broad line

     ;;Estimate line coverage
     coverage = qsfit_line_coverage(lines[i].wave, (lines[i].type EQ 'N' ? 1e3 : 1e4))

     ;;Ensure that the whole line has at least 60% of "good" channels
     IF (coverage LT 0.6) THEN BEGIN
        qsfit_log, 'The line ' + lines[i].name + ' is disabled'
        comp.enabled = 0
     ENDIF $
     ELSE  $
        comp.enabled = 1

     comp.center.val   = lines[i].wave
     comp.center.fixed = 1

     comp.v_off.val   = 0
     comp.v_off.fixed = 0
     ;;comp.v_off.step  = 100 ;;CUSTOMIZABLE

     comp.fwhm.fixed = 0

     IF (lines[i].type EQ 'N'  OR  lines[i].type EQ 'BN') THEN BEGIN
        comp.fwhm.val     =  500        ;CUSTOMIZABLE
        IF (lines[i].type EQ 'BN') THEN $
           comp.fwhm.limits  = [100, 1.e3] $ ;CUSTOMIZABLE
        ELSE $
           comp.fwhm.limits  = [100, 2e3] ;CUSTOMIZABLE
        comp.v_off.limits =  1000*[-1,1]  ;CUSTOMIZABLE

        ;;Guess normalization values
        comp.norm.val = 0
        IF (comp.enabled) THEN BEGIN
           dummy = MIN(ABS(xx - comp.center.val), ii)
           comp.norm.val  = ABS(yy[ii] - mo[ii])
           comp.norm.val /= qsfit_comp_emline(comp.center.val, 1, comp.center.val, 0, comp.fwhm.val)
        ENDIF

        gfit_add_comp, type=comp, 'na_' + lines[i].name
     ENDIF


     IF (lines[i].type EQ 'B'  OR  lines[i].type EQ 'BN') THEN BEGIN
        comp.fwhm.val     = 5000         ;CUSTOMIZABLE
        comp.fwhm.limits  = [900, 2.e4]  ;CUSTOMIZABLE
        comp.v_off.limits = 3000*[-1,1]  ;CUSTOMIZABLE

        IF (lines[i].name EQ 'MgII_2798') THEN $ ;;Exception for the "narrow" MgII line ;;TEST
           comp.v_off.limits  = 1000*[-1,1] ;;To avoid confusion with iron. CUSTOMIZABLE

        ;;Guess normalization values
        comp.norm.val = 0
        IF (comp.enabled) THEN BEGIN
           dummy = MIN(ABS(xx - comp.center.val), ii)
           comp.norm.val  = ABS(yy[ii] - mo[ii])
           comp.norm.val /= qsfit_comp_emline(comp.center.val, 1, comp.center.val, 0, comp.fwhm.val)
        ENDIF

        gfit_add_comp, type=comp, 'br_' + lines[i].name
     ENDIF
  ENDFOR

  ;;Add "unknwon" lines
  FOR i=0, qsfit_nunklines()-1 DO BEGIN
     comp.enabled = 0  ;;will be enabled in qsfit_add_unknown

     comp.center.val   = 3000 ;; will be set in qsfit_add_unknown
     comp.center.fixed = 0

     comp.fwhm.val     = 5000   ;CUSTOMIZABLE
     comp.fwhm.fixed   = 0
     comp.fwhm.limits  = [600, 1e4] ;CUSTOMIZABLE

     comp.v_off.val   = 0
     comp.v_off.fixed = 1

     gfit_add_comp, type=comp, 'unk' + gn2s(i+1)
  ENDFOR

  ;;Add expressions
  gfit_add_expr, 'expr_BroadLines', $
                 STRJOIN('br_' + lines[WHERE(lines.type EQ 'B'  OR  lines.type EQ 'BN')].name, ' + ')
  gfit.plot.(0).expr_broadlines.label = 'Broad'
  gfit.plot.(0).expr_broadlines.gp = 'w line ls 1 lw 2 lt rgb "blue"'

  gfit_add_expr, 'expr_NarrowLines', $
                 STRJOIN('na_' + lines[WHERE(lines.type EQ 'N'  OR  lines.type EQ 'BN')].name, ' + ')
  gfit.plot.(0).expr_narrowlines.label = 'Narrow'
  gfit.plot.(0).expr_narrowlines.gp = 'w line ls 1 lw 2 lt rgb "dark-red"'

  IF (qsfit_nunklines() GT 0) THEN $
     gfit_add_expr, 'expr_Unknown', STRJOIN('unk' + gn2s(INDGEN(qsfit_nunklines())+1), ' + ') $
  ELSE $
     gfit_add_expr, 'expr_Unknown', '0'
  tmp = N_TAGS(gfit.plot.(0)) - 1
  gfit.plot.(0).expr_Unknown.plot  = 0 ;;will be enabled in qsfit_add_unknown
  gfit.plot.(0).expr_Unknown.label = 'Unknown'
  gfit.plot.(0).expr_Unknown.gp = 'w line ls 1 lw 1 lt rgb "purple"'

  ;;Tie narrow components (CUSTOMIZABLE)
  qsfit_log, 'The velocity offsets of [OIII4959] and [OIII5007] are tied'
  gfit.comp.na_OIII_4959.v_off.tied = 'na_OIII_5007.v_off'

  qsfit_compile
  qsfit_freeze, line=0
END



;=====================================================================
;NAME:
;  qsfit_add_iron
;
;PURPOSE:
;  Add the optical and UV iron template components to the GFIT model.
;
;PARAMETERS:
;  NONE.
;
PRO qsfit_add_iron
  COMPILE_OPT IDL2
  ON_ERROR, !glib.on_error
  COMMON GFIT

  ;;Add UV iron template (needed to properly fit the SBB)
  gprint, 'Adding opt/UV iron templates...'
  ironuv = gfit_component('qsfit_comp_ironuv')

  ironuv.norm.val    = 100
  ironuv.norm.fixed  = 0
  ironuv.fwhm.val    = 3000
  ironuv.fwhm.fixed  = 1
  ironuv.fwhm.limits = [1e3, 1e4]
  ironuv.fwhm.step   = 500

  ironuv.opt.part=0
  gfit_add_comp, type=ironuv, 'ironuv0'

  ironuv.opt.part=1
  gfit_add_comp, type=ironuv, 'ironuv1'

  ironuv.opt.part=2
  gfit_add_comp, type=ironuv, 'ironuv2'

  ironuv.opt.part=3
  gfit_add_comp, type=ironuv, 'ironuv3'


  tmp = INT_TABULATED(gfit.cmp.(0).x, qsfit_comp_ironuv(gfit.cmp.(0).x, 1, 1000, part=0))
  IF (tmp LT 0.5) THEN BEGIN
     qsfit_log, 'Iron UV part 0 is disabled'
     gfit.comp.ironuv0.enabled = 0
  ENDIF
  tmp = INT_TABULATED(gfit.cmp.(0).x, qsfit_comp_ironuv(gfit.cmp.(0).x, 1, 1000, part=1))
  IF (tmp LT 0.5) THEN BEGIN
     qsfit_log, 'Iron UV part 1 is disabled'
     gfit.comp.ironuv1.enabled = 0
  ENDIF
  tmp = INT_TABULATED(gfit.cmp.(0).x, qsfit_comp_ironuv(gfit.cmp.(0).x, 1, 1000, part=2))
  IF (tmp LT 0.5) THEN BEGIN
     qsfit_log, 'Iron UV part 2 is disabled'
     gfit.comp.ironuv2.enabled = 0
  ENDIF
  tmp = INT_TABULATED(gfit.cmp.(0).x, qsfit_comp_ironuv(gfit.cmp.(0).x, 1, 1000, part=3))
  IF (tmp LT 0.5) THEN BEGIN
     qsfit_log, 'Iron UV part 3 is disabled'
     gfit.comp.ironuv3.enabled = 0
  ENDIF


  ;;Add optical iron template
  ironopt = gfit_component('qsfit_comp_ironoptical')
  gfit_add_comp, type=ironopt, 'ironopt'

  tmp = MAX(gfit.cmp.(0).x)
  IF (tmp LT 4600) THEN BEGIN
     qsfit_log, 'Opt. iron component is disabled since MAX(lambda)=' + gn2s(tmp) + ' < 4600AA'
     gfit.comp.ironopt.enabled = 0
  ENDIF

  gfit.comp.ironopt.norm_br.val   = 0.1
  gfit.comp.ironopt.norm_na.val   = 0.1
  gfit.comp.ironopt.norm_br.fixed = 0
  gfit.comp.ironopt.norm_na.fixed = 0

  gfit.comp.ironopt.fwhm_br.val    = 3000
  gfit.comp.ironopt.fwhm_br.fixed  = 1
  gfit.comp.ironopt.fwhm_br.limits = [1e3, 1e4]
  gfit.comp.ironopt.fwhm_br.step   = 500

  gfit.comp.ironopt.fwhm_na.val    = 500
  gfit.comp.ironopt.fwhm_na.fixed  = 1
  gfit.comp.ironopt.fwhm_na.limits = [200, 1e3]
  gfit.comp.ironopt.fwhm_na.step   = 200


  ;;Add expression for iron templates
  gfit_add_expr, 'expr_Iron', 'ironuv0 + ironuv1 + ironuv2 + ironuv3 + ironopt'
  gfit.plot.(0).expr_iron.label = 'Iron'
  gfit.plot.(0).expr_iron.gp = 'w line ls 1 lw 1 lt rgb "dark-green"'


  qsfit_compile
END



;=====================================================================
;NAME:
;  qsfit_add_unknown
;
;PURPOSE:
;  Enable the "unknown" line components and sets their center
;  wavelength where there is a maximum in the fitting residuals.
;
;PARAMETERS:
;  NONE.
;
PRO qsfit_add_unknown
  COMPILE_OPT IDL2
  ON_ERROR, !glib.on_error
  COMMON GFIT

  ;;Set "unknown" line center wavelength where there is a maximum in
  ;;the fit residuals, and re-run a fit.
  iadd = 0l
  FOR iiunk=1, qsfit_nunklines() DO BEGIN
     iunk = WHERE(TAG_NAMES(gfit.comp) EQ 'UNK' + gn2s(iiunk))

     ;;Search for a maximum in the residual
     gfit_eval
     xx = gfit.cmp.(0).x
     yy = gfit.cmp.(0).y
     ee = gfit.cmp.(0).e
     mo = gfit.cmp.(0).m

     ;;Do not add lines within 6000 km/s from the edges since these
     ;;may influence continuum fitting (6000 km/s / speed of light =
     ;;0.02).
     range = gminmax(xx)
     range *= (1 + [1,-1]*0.02) ;; CUSTOMIZABLE
     ii = WHERE(xx LT range[0]   OR   xx GT range[1])
     IF (ii[0] NE -1) THEN mo[ii] = yy[ii]

     REPEAT BEGIN
        maxresid = MAX((yy - mo) / ee, imax)
        IF (maxresid LE 0) THEN BEGIN
           gprint, 'No residual is greater than 0, skip searching further residuals.'
           RETURN
        ENDIF

        mo[imax] = yy[imax] ;;Avoid considering again the same residual

     ;;New line must be at least 2 sample away from previously
     ;;considered residual.
     ENDREP UNTIL (MIN(ABS(imax - iadd)) GT 2)

     ;;Add an emission line
     iadd = [iadd, imax]
     xadd = xx[imax]

     gprint, 'Enabling "unknown" em. line ' + $
             gn2s(iiunk) + '/' + gn2s(qsfit_nunklines()) + ' at ', xadd
     gfit.comp.(iunk).enabled       = 1
     gfit.comp.(iunk).center.val    = xadd
     gfit.comp.(iunk).center.limits = xadd + xadd/10.*[-1,1] ;allow to move 10%
     gfit.comp.(iunk).v_off.val     = 0
     gfit.comp.(iunk).v_off.fixed   = 1

     ;;Fit
     qsfit_compile
     gfit_run
     qsfit_freeze, lines=1 ;;freeze emission lines parameters

     ;;If the last maximum residual was less than 3 sigma exit the loop
     IF (maxresid LT 3) THEN BREAK ;;CUSTOMIZABLE
  ENDFOR

  ;;Enable plotting of unknown lines
  gfit.plot.(0).expr_Unknown.plot = 1
END



;=====================================================================
;NAME:
;  qsfit_estimate_fwhm_voff
;
;PURPOSE:
;  Estimate the FWHM and velocity offset of an emission line.  The
;  line profile is not required to be Gaussian or symmetric, however
;  it must have one absolute maximum and no secondary (or local)
;  maxima.
;
;PARAMETERS:
;  _X: (input, an array of numbers)
;    The array of wavelengths.
;
;  _Y: (input, an array of numbers)
;    The array of flux density, or luminosity density, with the
;    emission line profile.
;
;  CENTER: (input, a scalar number)
;    The assumed center wavelength of the emission line. The velocity
;    offset is calculated with respect to this wavelength. The units
;    are the same as _X.
;
;RETURN VALUE: (a scalar structure)
;  A stucture with the following tags:
;    - FWHM: the full width at half maximum of the line profile
;      calculated as (DeltaX / Xmax), DeltaX is the width at half
;      maximum and Xmax is the wavelength of the peak.
;
;    - VOFF: the velocity offset of the line profile calculated as
;      (CENTER - Xmax) / CENTER, where Xmax is the wavelength of the
;      peak.
;
;EXAMPLE:
;  center = 100.
;  sigma = 2.
;  x = ggen(-3*sigma, 3*sigma, 1000) + center
;  y = ggauss(x, center, sigma) + $
;      ggauss(x, center+0.7*sigma, sigma/2)
;
;  gps, qsfit_estimate_fwhm_voff(x, y, center, /plot)
;
FUNCTION qsfit_estimate_fwhm_voff, _x, _y, center, PLOT=plot
  COMPILE_OPT IDL2
  ON_ERROR, !glib.on_error

  plot = KEYWORD_SET(plot)

  x = ggen(gminmax(_x), gn(_x)*10)
  y = INTERPOL(_y, _x, x)

  dummy = MIN(ABS(x - center), icen)
  max = MAX(y, imax)

  ihw1 = MAX(WHERE(y[0:imax-1] LT max/2))
  ihw2 = MIN(WHERE(y[imax:*]   LT max/2))
  IF ((imax LT 10)         OR   $
      (imax GT gn(y)-10)   OR   $
      (ihw1 EQ -1     )    OR   $
      (ihw2 EQ -1))        THEN BEGIN
     gprint, 'The peak is too close to the edge, could not esitmate FWHM for emission line at ' + gn2s(center) + ' AA'
     RETURN, ''
  ENDIF
  ihw2 += imax

  IF (plot) THEN BEGIN
     xr = x[gminmax(WHERE(y GT max/100))]
     ggp_cmd, /clear, xr=xr, ['unset grid', 'set key left'], $
              xtit='Wavelength [arb. units]', ytit='Flux density (F_{/Symbol l}) [arb. units]'
     ggp_data, x, y                         , pl='w l t "Em. line" lw 2 lc rgb "black"'
     ggp_data, x[icen]*[1,1], [0,max]       , pl='w l dt 5 lw 2 lc rgb "red" t "{/Symbol l}_{Ref}"'
     ggp_data, [x[icen], x[imax]], max*[1,1], pl='w l lt 2 lw 2 lc rgb "red" t "V_{off}"'
     ggp_data, xr, max/2*[1,1]              , pl='w l t "Half max" lw 2 dt 2 lc rgb "dark-green"'
     ggp_data, x[ihw1]*[1,1], max*[0,1.2]   , pl='w l t "FWHM" dt 4 lw 2 lc rgb "blue"'
     ggp_data, x[ihw2]*[1,1], max*[0,1.2]   , pl='w l notitle  dt 4 lw 2 lc rgb "blue"'
     ggp;;, term='pdf', output='fwhm_voff.pdf'
  ENDIF

  fwhm = (x[ihw2] - x[ihw1]) / x[imax]
  voff = (x[icen] - x[imax]) / x[icen]
  gassert, fwhm GT 0

  ret = {fwhm: fwhm, voff: voff}

  RETURN, ret
END



;=====================================================================
;NAME:
;  qsfit_reduce_line_templ
;
;PURPOSE:
;  Return a template structure to describe the properties of an
;  emission line.  This template is used in qsfit_reduce_line().
;
;PARAMETERS:
;  NONE.
;
;RETURN VALUE: (a scalar structure)
;  Return the template structure.
;
FUNCTION qsfit_reduce_line_templ
  line = { ncomp: 0b $
           , lum:  gnan(), lum_err:  gnan()  $
           , fwhm: gnan(), fwhm_err: gnan()  $
           , voff: gnan(), voff_err: gnan()  $
           , quality: 0b                     $
         }
  RETURN, line
END


;=====================================================================
;NAME:
;  qsfit_line_quality_meaning
;
;PURPOSE:
;  Return a message explaining the meaning of a bit in the emission
;  line quality value.
;
;PARAMETERS:
;  BIT (input, a scalar byte)
;    The bit to whose meaning is to be retrieved.  The value must be
;    in the range 0:7.
;
;RETURN VALUE: (a scalar string)
;  A string explaining the meaning of a bit in the quality value.
;
FUNCTION qsfit_line_quality_meaning, bit
  CASE bit OF
     0: RETURN, 'Bit 0: either the luminosity or its uncertainty are NaN'
     1: RETURN, 'Bit 1: luminosity relative uncertainty > 1.5'
     2: RETURN, 'Bit 2: either the FWHM or its uncertainty are NaN'
     3: RETURN, 'Bit 3: FWHM value hits a limit in the fit'
     4: RETURN, 'Bit 4: FWHM relative uncertainty > 2'
     5: RETURN, 'Bit 5: either the Voff or its uncertainty are NaN'
     6: RETURN, 'Bit 6: Voff value hits a limit in the fit'
     7: RETURN, 'Bit 7: Voff uncertainty > 500 km s^-1'
     ELSE: RETURN, 'Bit ' + gn2s(bit) + ' is not used in emission line quality value'
  ENDCASE
END



;=====================================================================
;NAME:
;  qsfit_reduce_line
;
;PURPOSE:
;  Calculate the luminosity, FWHM and velocity offset of an emission
;  line.
;
;DESCRIPTION:
;  This procedure checks whether one (or more) "unknown" component
;  lines can be associated to a "known" emission line.  The
;  association is confirmed if the center of the "known" emission
;  lines lies within the FWHM of the "unknown" line.
;
;  If one (or more) lines are associated the new line profile is
;  computed and the total luminosity, FWHM and velocity offset are
;  computed.
;
;  If no "unknown" line is associated the luminosity, FWHM and
;  velocity offset are those of the simple "qsfit_comp_emline"
;  component.
;
;PARAMETERS:
;  CNAME: (input, a scalar string)
;    The name of the GFIT component of type "qsfit_comp_emline" to
;    be analyzed.
;
;  TYPE: (input, a scalar string)
;    Either "B" or "N", to specify whether the line is a "broad" or
;    "narrow" emission line.
;
;  WAVE: (input, a scalar number)
;    The wavelength of the emission line to be analyzed (in
;    Angstrom).
;
;RETURN VALUE:
;  (a scalar structure whose template is given by
;  qsfit_reduce_line_templ()) A structure containing the luminosity,
;  FWHM and velocity offset of an emission line, and the associated
;  uncertainties.
;
FUNCTION qsfit_reduce_line, cname, wave
  COMPILE_OPT IDL2
  ON_ERROR, !glib.on_error
  COMMON GFIT

  ;;Emission line FWHM broad/narrow separator
  brnasep = 1000 ;;CUSTOMIZABLE

  ;;Search component in the model
  icomp = WHERE(TAG_NAMES(gfit.comp) EQ STRUPCASE(cname))
  IF (icomp[0] EQ -1) THEN $
     MESSAGE, 'No component named: ' + cname

  ;;If component is disabled just return an empty structure
  IF (~gfit.comp.(icomp).enabled) THEN $
     RETURN, qsfit_reduce_line_templ()

  isNarrow = (STRMID(cname, 0, 2) EQ 'na')

  ;;Calculate actual emission line center (taking into account V_OFF)
  center = gfit.comp.(icomp).center.val
  v_off  = gfit.comp.(icomp).v_off.val
  wave_center = center * (1. + (v_off / 3.e5))

  ;;Index of "unknown" lines
  iunk = []
  IF (qsfit_nunklines() GT 0) THEN $
     iunk = WHERE(STRMID(TAG_NAMES(gfit.comp), 0, 3) EQ 'UNK')

  ;;Loop through "unknown" lines to check if an "unknown" line can be
  ;;associated to current line
  assoc = icomp
  FOR i=0, gn(iunk)-1 DO BEGIN
     j = iunk[i]
     IF (~gfit.comp.(j).enabled) THEN CONTINUE

     ;;Get center and FWHM of the unknown line
     center  = gfit.comp.(j).center.val
     fwhm    = gfit.comp.(j).fwhm.val
     fwhm_aa = fwhm * center / 3.e5 ;;FWHM in Angstrom

     ;;Currently we avoid association of unknown lines to narrow
     ;;lines.
     IF (isNarrow) THEN CONTINUE

     IF (~isNarrow   AND   (fwhm LT brnasep)) THEN CONTINUE
     IF ( isNarrow   AND   (fwhm GE brnasep)) THEN CONTINUE
     IF (gfit.comp.(j).norm.val EQ 0)         THEN CONTINUE


     ;;Check if the "unknown" line is sufficiently close to the
     ;;emission line center.
     IF (ABS(center - wave_center) LT fwhm_aa/2) THEN BEGIN
        assoc = [assoc, j]
     ENDIF
  ENDFOR

  ;;Prepare return value
  line = qsfit_reduce_line_templ()

  ;;If no association is made we just return the GFIT values
  IF (gn(assoc) EQ 1) THEN BEGIN
     line.ncomp    = 1
     line.lum      = gfit.comp.(assoc).norm.val
     line.lum_err  = gfit.comp.(assoc).norm.err
     line.fwhm     = gfit.comp.(assoc).fwhm.val
     line.fwhm_err = gfit.comp.(assoc).fwhm.err
     line.voff     = gfit.comp.(assoc).v_off.val
     line.voff_err = gfit.comp.(assoc).v_off.err
  ENDIF $
  ELSE BEGIN
     ;;At least two line components are going to be associated: save the
     ;;GFIT state before proceeding...
     saved_gfit = gfit

     ;;Collect all associated emission line components and disable the
     ;;other ones
     emline = []
     FOR ii=0, gfit.comp.nn-1 DO BEGIN
        ;;If ii-th component is one of the components to be associated...
        IF (gsearch(assoc EQ ii)) THEN BEGIN

           ;;Take care of tied parameters!
           FOR jj=0, gfit.comp.(ii).npar-1 DO BEGIN
              tied = gfit.comp.(ii).(jj).tied
              IF (tied NE '') THEN BEGIN
                 qsfit_log, 'WARNING: parameter ' + $
                           gfit.comp.(ii).(jj).comp + '.'  + $
                           gfit.comp.(ii).(jj).parname     + $
                           ' is tied to: ' + tied
              ENDIF
           ENDFOR

           emline = [emline, gfit.comp.(ii)]
        ENDIF $
        ELSE BEGIN
           ;;Non-associated components are disabled and their value
           ;;set to 0.
           gfit.comp.(ii).enabled = 0
           gfit.comp.(ii).disabled_val = 0
        ENDELSE
     ENDFOR

     ;;There must be at least one associated line with reliable
     ;;uncertainty
     ii = WHERE(FINITE(emline.norm.err))
     gassert, ii[0] NE -1
     emline = emline[ii]

     ;;Create a model expression for the sum of all associated lines
     gfit_add_expr, 'assoc_sum', STRJOIN((TAG_NAMES(gfit.comp))[assoc], ' + ')
     qsfit_compile
     gfit_eval, expr=sum
     sum = sum.(0).(N_TAGS(sum.(0))-1)

     ;;Log parameters of individual components
     qsfit_log, 'Line ' + cname + ' (wavelength=' + gn2s(wave) + ')' $
               + '  is modeled with ' + gn2s(gn(assoc)) + ' components:'
     par = gfit_get_par(compenabled)
     par = par[WHERE(compenabled)]
     gps, par, out=tmp
     qsfit_log, tmp
     qsfit_log

     ;;Reduce line
     res = qsfit_estimate_fwhm_voff(gfit.cmp.(0).x, sum, wave)

     IF (gtype(res) EQ 'STRUCT') THEN BEGIN
        line.ncomp   = gn(assoc)

        weight = emline.norm.val
        weight /= TOTAL(weight)
        line.lum     = TOTAL(emline.norm.val)
        line.lum_err = TOTAL(weight * emline.norm.err)
        line.fwhm    = res.fwhm * 3.e5
        line.voff    = res.voff * 3.e5

        ;;Estimate errors on associated lines by weighting errors on
        ;;individual components
        err = emline.fwhm.err
        ii = WHERE(err GT 0)
        IF (ii[0] NE -1) THEN BEGIN
           weight = emline[ii].norm.val
           weight /= TOTAL(weight)
           line.fwhm_err = TOTAL(err[ii] * weight)
        ENDIF $
        ELSE BEGIN
           line.fwhm     = gnan()
           line.fwhm_err = gnan()
        ENDELSE

        err = emline.v_off.err
        ii = WHERE(err GT 0)
        IF (ii[0] NE -1) THEN BEGIN
           weight = emline[ii].norm.val
           weight /= TOTAL(weight)
           line.voff_err = TOTAL(err[ii] * weight)
        ENDIF $
        ELSE BEGIN
           line.voff     = gnan()
           line.voff_err = gnan()
        ENDELSE
     ENDIF

     ;;Restore GFIT status
     gfit = saved_gfit
     qsfit_compile
     gfit = saved_gfit ;;qsfit_compile deletes the gfit.res data, hence I copy it again
     emline = []       ;;no longer needed
  ENDELSE


  ;;Check output
  IF (FINITE(line.lum))      THEN gassert, line.lum      GE 0
  IF (FINITE(line.lum_err )) THEN gassert, line.lum_err  GE 0
  IF (FINITE(line.fwhm))     THEN gassert, line.fwhm     GE 0
  IF (FINITE(line.fwhm_err)) THEN gassert, line.fwhm_err GE 0
  IF (FINITE(line.voff_err)) THEN gassert, line.voff_err GE 0

  ;;Set bits in the QUALITY field
  line.quality = 0
  IF (~FINITE(line.lum)       OR    $
      ~FINITE(line.lum_err)   OR    $
      (line.lum     LE 0)     OR    $
      (line.lum_err LE 0)     ) THEN BEGIN
     line.quality += 1
  ENDIF $
  ELSE BEGIN
     IF (line.lum_err/line.lum GT 1.5) THEN line.quality += 2
  ENDELSE

  IF (~FINITE(line.fwhm)       OR    $
      ~FINITE(line.fwhm_err)   OR    $
      (line.fwhm     LE 0)     OR    $
      (line.fwhm_err LE 0)     ) THEN BEGIN
     line.quality += 4
  ENDIF $
  ELSE BEGIN
     IF (gn(assoc) EQ 1) THEN BEGIN
        IF (MIN(ABS(gfit.comp.(assoc).fwhm.val - gfit.comp.(assoc).fwhm.limits), /nan) LT 200) THEN $
           line.quality += 8
     ENDIF
     IF (line.fwhm_err/line.fwhm GT 2) THEN line.quality += 16
  ENDELSE

  IF (~FINITE(line.voff)       OR    $
      ~FINITE(line.voff_err)   OR    $
      (line.voff     EQ 0)     OR    $
      (line.voff_err LE 0)     ) THEN BEGIN
     line.quality += 32
  ENDIF $
  ELSE BEGIN
     IF (gn(assoc) EQ 1) THEN BEGIN
        IF (MIN(ABS(gfit.comp.(assoc).v_off.val - gfit.comp.(assoc).v_off.limits), /nan) LT 100) THEN $
           line.quality += 64
     ENDIF
     IF (line.voff_err GT 500) THEN line.quality += 128
  ENDELSE


  ;;Log QUALITY value
  IF (line.quality NE 0) THEN BEGIN
     type = 'broad'
     IF (isNarrow) THEN type = 'narrow'
     qsfit_log, 'Line ' + cname + ' (' + type + ', wl=' + gn2s(wave) + ')'
     FOR b=0, 7 DO BEGIN
        IF ((line.quality AND 2b^b) GT 0) THEN $
           qsfit_log, '  ' + qsfit_line_quality_meaning(b)
     ENDFOR
  ENDIF

  RETURN, line
END








;=====================================================================
;NAME:
;  qsfit_cont_quality_meaning
;
;PURPOSE:
;  Return a message explaining the meaning of a bit in the continuum
;  quality value.
;
;PARAMETERS:
;  BIT (input, a scalar byte)
;    The bit to whose meaning is to be retrieved.  The value must be
;    in the range 0:5.
;
;RETURN VALUE: (a scalar string)
;  A string explaining the meaning of a bit in the quality value.
;
FUNCTION qsfit_cont_quality_meaning, bit
  CASE bit OF
     0: RETURN, 'Bit 0: wavelength is outside the observed range'
     1: RETURN, 'Bit 1: either the luminosity or its uncertainty are NaN'
     2: RETURN, 'Bit 2: luminosity relative uncertainty > 1.5'
     3: RETURN, 'Bit 3: either the slope or its uncertainty are NaN'
     4: RETURN, 'Bit 4: slope hits a limit in the fit'
     5: RETURN, 'Bit 5: slope uncertainty > 0.3'
     6: RETURN, 'Bit 6: host galaxy / continuum > 0.5'
     ELSE: RETURN, 'Bit ' + gn2s(bit) + ' is not used in continuum quality value'
  ENDCASE
END


;=====================================================================
;NAME:
;  qsfit_galaxy_quality_meaning
;
;PURPOSE:
;  Return a message explaining the meaning of a bit in the galaxy
;  quality value.
;
;PARAMETERS:
;  BIT (input, a scalar byte)
;    The bit to whose meaning is to be retrieved.  The value must be
;    in the range 0:2.
;
;RETURN VALUE: (a scalar string)
;  A string explaining the meaning of a bit in the quality value.
;
FUNCTION qsfit_galaxy_quality_meaning, bit
  CASE bit OF
     0: RETURN, 'Bit 0: fit of galaxy template is not sensible at this redshift'
     1: RETURN, 'Bit 1: either the luminosity or its uncertainty are NaN'
     2: RETURN, 'Bit 2: luminosity relative uncertainty > 1.5'
     ELSE: RETURN, 'Bit ' + gn2s(bit) + ' is not used in emission galaxy quality value'
  ENDCASE
END


;=====================================================================
;NAME:
;  qsfit_iron_quality_meaning
;
;PURPOSE:
;  Return a message explaining the meaning of a bit in the iron
;  quality value.
;
;PARAMETERS:
;  BIT (input, a scalar byte)
;    The bit to whose meaning is to be retrieved.  The value must be
;    in the range 0:2.
;
;RETURN VALUE: (a scalar string)
;  A string explaining the meaning of a bit in the quality value.
;
FUNCTION qsfit_iron_quality_meaning, bit
  CASE bit OF
     0: RETURN, 'Bit 0: fit of iron template is not sensible at this redshift'
     1: RETURN, 'Bit 1: either the luminosity or its uncertainty are NaN'
     2: RETURN, 'Bit 2: luminosity relative uncertainty > 1.5'
     ELSE: RETURN, 'Bit ' + gn2s(bit) + ' is not used in emission iron quality value'
  ENDCASE
END


;=====================================================================
;NAME:
;  qsfit_reduce
;
;PURPOSE:
;  Reduce the GFIT results and return a standardized structure with
;  all relevant quantities as well as the GFIT structure.
;
;RETURN VALUE: (scalar structure)
;    A structure with all the relevant spectral quantities derived
;    from GFIT results.
;
FUNCTION qsfit_reduce
  COMPILE_OPT IDL2
  ON_ERROR, !glib.on_error
  COMMON GFIT

  ;;Evaluate expressions
  gfit_eval, expr=expr

  ;;Prepare return structure
  qsfit_log, out=log
  out  = { qsfit_version: qsfit_version()                       , $
           file_output:   ''                                    , $
           gfit:          gfit                                  , $
           expr:          expr.(0)                              , $
           log:           log                                   , $
           ndata:         gn(gfit.cmp.(0).x)                    , $
           good_fraction: gfit.data.(0).udata.goodFraction      , $
           median_flux:   FLOAT(gfit.data.(0).udata.median_flux), $
           median_err:    FLOAT(gfit.data.(0).udata.median_err)   $
         }


  ;;--------------------------
  ;;Reduce galaxy template data
  galaxy= { lum:     gfit.comp.galaxy.norm.val * 5500, $
            lum_err: gfit.comp.galaxy.norm.err * 5500, $
            quality: 0b      $
          }

  IF (~gfit.comp.galaxy.enabled) THEN BEGIN
     galaxy.lum     = gnan()
     galaxy.lum_err = gnan()
     galaxy.quality = 1
  ENDIF $
  ELSE BEGIN
     IF (~FINITE(galaxy.lum)       OR    $
         ~FINITE(galaxy.lum_err)   OR    $
         (galaxy.lum     LE 0)     OR    $
         (galaxy.lum_err LE 0)     ) THEN BEGIN
        galaxy.quality = 2
     ENDIF $
     ELSE BEGIN
        IF (galaxy.lum_err / galaxy.lum GT 1.5) THEN BEGIN ;;CUSTOMIZABLE
           galaxy.quality += 4
        ENDIF
     ENDELSE
  ENDELSE
  out = CREATE_STRUCT(out, 'galaxy', galaxy)

  ;;Log QUALITY value
  IF (galaxy.quality NE 0) THEN BEGIN
     qsfit_log, 'Host galaxy:'
     FOR b=0, 2 DO BEGIN
        IF ((galaxy.quality AND 2b^b) GT 0) THEN $
           qsfit_log, '  ' + qsfit_galaxy_quality_meaning(b)
     ENDFOR
  ENDIF



  ;;--------------------------
  ;;Reduce continuum data

  ;;Evalute the continuum luminosity and slope at NCONT equally spaced
  ;;wavelengths (in the rest frame)
  ncont = 5

  ;;Also, evaluate continuum luminosty and slopes at these fixed
  ;;wavelengths:
  fixed_wave = [1450, 2245, 3000, 4210, 5100]

  ;;Generate equally spaced logarithmic wavelengths within the rest
  ;;frame range
  xx = gfit.cmp.(0).x
  range = gminmax(xx)
  range += (range[1] - range[0]) / 20. * [1, -1] ;;Restrict range by 5%
  range = gloggen(range, ncont)
  wavelengths = [range, fixed_wave]

  ;;Consider just the SBPL component to evaluate the slopes
  sbpl = out.expr.expr_continuum
  i = WHERE(sbpl LE 0)
  gassert, i[0] EQ -1
  slopes = DERIV(ALOG10(xx), ALOG10(sbpl))

  ;;Result structure
  cont = { wave:      gnan(), $ ;;[Angstrom]
           lum:       gnan(), $ ;;nu L_nu = lambda L_lambda [10^42 erg s^-1]
           lum_err:   gnan(), $
           slope:     gnan(), $ ;;L_lambda \propto lambda^slope
           slope_err: gnan(), $
           galaxy:    gnan(), $
           quality:   0b      $
         }

  ;;Loop through continuum wavelengths
  allcont = REPLICATE(cont, ncont+gn(fixed_wave))
  FOR j=0, ncont + gn(fixed_wave)-1 DO BEGIN
     ;;Set default values
     cont.wave      = wavelengths[j]
     cont.lum       = gnan()
     cont.lum_err   = gnan()
     cont.slope     = gnan()
     cont.slope_err = gnan()
     cont.galaxy    = gnan()
     cont.quality   = 0

     ;;Points outside wavelength range are unreliable, skip them
     IF (cont.wave LT wavelengths[0]  OR  cont.wave GT wavelengths[ncont-1]) THEN BEGIN
        cont.quality = 1
     ENDIF $
     ELSE BEGIN
        ;;Compute continuum luminosity
        cont.lum    = INTERPOL(out.expr.expr_continuum, xx, cont.wave) * cont.wave
        cont.galaxy = gnan()
        IF (gfit.comp.galaxy.enabled) THEN $
           cont.galaxy = INTERPOL(out.expr.expr_galaxy, xx, cont.wave) * cont.wave

        cont.lum_err = gfit.comp.continuum.norm.err * cont.wave
        IF (~FINITE(cont.lum)            OR    $
            ~FINITE(cont.lum_err)        OR    $
            (cont.lum LE 0)              OR    $
            (cont.lum_err LE 0))  THEN BEGIN
           cont.lum     = gnan()
           cont.lum_err = gnan()
           cont.quality = 2
        ENDIF $
        ELSE  BEGIN
           IF (cont.lum_err/cont.lum GT 1.5) THEN cont.quality += 4

           ;;Compute slope
           cont.slope = INTERPOL(slopes, xx, cont.wave)

           ;;Compute slope uncertainty as the sum of the two slopes
           ;;uncertainties.
           cont.slope_err = gnan()
           IF (FINITE(gfit.comp.continuum.alpha1.err)) THEN BEGIN
              cont.slope_err = gfit.comp.continuum.alpha1.err
              IF (FINITE(gfit.comp.continuum.dalpha.err)) THEN $
                 cont.slope_err += gfit.comp.continuum.dalpha.err
           ENDIF

           IF (~FINITE(cont.slope)            OR    $
               ~FINITE(cont.slope_err)        OR    $
               (ABS(cont.slope) EQ 0)         OR    $
               (cont.slope_err LE 0))  THEN BEGIN
              cont.slope     = gnan()
              cont.slope_err = gnan()
              cont.quality  += 8
           ENDIF $
           ELSE BEGIN
              IF (MIN(ABS(cont.slope - gfit.comp.continuum.alpha1.limits), /nan) LT 0.05) THEN $
                 cont.quality += 16
              IF (cont.slope_err GT 0.3) THEN cont.quality += 32
           ENDELSE
        ENDELSE

        IF (gfit.comp.galaxy.enabled) THEN BEGIN
           IF (cont.galaxy GT 0.5*cont.lum) THEN $
              cont.quality += 64
        ENDIF
     ENDELSE

     ;;Log QUALITY value
     IF (cont.quality NE 0) THEN BEGIN
        qsfit_log, 'Continuum at ' + gn2s(cont.wave) +' AA'
        FOR b=0, 6 DO BEGIN
           IF ((cont.quality AND 2b^b) GT 0) THEN $
              qsfit_log, '  ' + qsfit_cont_quality_meaning(b)
        ENDFOR
     ENDIF

     ;;Save result
     allcont[j] = cont
  ENDFOR

   ;;Check if at least one continuum estimate has quality=64 (host
   ;;galaxy / continuum > 0.5).  In these sources ALL the slopes are likely
   ;;biased.
   slopeBiased = 0
   FOR i=0, ncont + gn(fixed_wave)-1 DO BEGIN
      IF ((allcont[i].quality AND 2b^6) GT 0) THEN $
         slopeBiased = 1
   ENDFOR

   IF (slopeBiased) THEN BEGIN
      FOR i=0, ncont + gn(fixed_wave)-1 DO BEGIN
         IF ((allcont[i].quality AND 2b^6) EQ 0) THEN $
            allcont[i].quality += 64
      ENDFOR
   ENDIF


  ;;Save the continuum estimates individually
  FOR j=0, ncont-1 DO $
     out = CREATE_STRUCT(out, 'cont'+gn2s(j+1), allcont[j])
  FOR j=ncont, ncont+gn(fixed_wave)-1 DO $
     out = CREATE_STRUCT(out, 'cont'+gn2s(ROUND(allcont[j].wave)), allcont[j])

  ;;Save also all the continuum estimates as an array
  out = CREATE_STRUCT(out, 'cont', allcont)



  ;;--------------------------
  ;;Reduce iron templates data
  gassert, gfit.comp.ironuv0.norm.val    GE 0
  gassert, gfit.comp.ironuv1.norm.val    GE 0
  gassert, gfit.comp.ironuv2.norm.val    GE 0
  gassert, gfit.comp.ironuv3.norm.val    GE 0
  gassert, gfit.comp.ironopt.norm_br.val GE 0
  gassert, gfit.comp.ironopt.norm_na.val GE 0


  FOR iPart=0, 3 DO BEGIN
     i = WHERE(TAG_NAMES(gfit.comp) EQ 'IRONUV' + gn2s(iPart))
     gassert, i[0] NE -1

     ;;Result structure
     iron = { lum:      gfit.comp.(i).norm.val, $
              lum_err:  gfit.comp.(i).norm.err, $
              fwhm:     gfit.comp.(i).fwhm.val, $
              fwhm_err: gfit.comp.(i).fwhm.err, $
              quality:  0b                      $
            }

     IF (~gfit.comp.(i).enabled) THEN BEGIN
        iron.lum      = gnan()
        iron.lum_err  = gnan()
        iron.fwhm     = gnan()
        iron.fwhm_err = gnan()
        iron.quality  = 1
     ENDIF $
     ELSE BEGIN
        IF (~FINITE(iron.lum)       OR    $
            ~FINITE(iron.lum_err)   OR    $
            (iron.lum     LE 0)     OR    $
            (iron.lum_err LE 0)     ) THEN BEGIN
           iron.quality = 2
        ENDIF $
        ELSE BEGIN
           IF (iron.lum_err / iron.lum GT 1.5) THEN BEGIN ;;CUSTOMIZABLE
              iron.quality += 4
           ENDIF
        ENDELSE
     ENDELSE
     out = CREATE_STRUCT(out, 'ironuv' + gn2s(iPart), iron)

     ;;Log QUALITY value
     IF (iron.quality NE 0) THEN BEGIN
        qsfit_log, 'Iron emission lines (UV, part ' + gn2s(iPart) + '):'
        FOR b=0, 2 DO BEGIN
           IF ((iron.quality AND 2b^b) GT 0) THEN $
              qsfit_log, '  ' + qsfit_iron_quality_meaning(b)
        ENDFOR
     ENDIF
  ENDFOR


  ;;Result structure
  iron = { lum:      gfit.comp.ironopt.norm_br.val, $
           lum_err:  gfit.comp.ironopt.norm_br.err, $
           fwhm:     gfit.comp.ironopt.fwhm_br.val, $
           fwhm_err: gfit.comp.ironopt.fwhm_br.err, $
           quality:  0b                             $
         }

  IF (~gfit.comp.ironopt.enabled) THEN BEGIN
     iron.lum      = gnan()
     iron.lum_err  = gnan()
     iron.fwhm     = gnan()
     iron.fwhm_err = gnan()
     iron.quality  = 1
  ENDIF $
  ELSE BEGIN
     IF (~FINITE(iron.lum)       OR    $
         ~FINITE(iron.lum_err)   OR    $
         (iron.lum     LE 0)     OR    $
         (iron.lum_err LE 0)     ) THEN BEGIN
        iron.quality = 2
     ENDIF $
     ELSE BEGIN
        IF (iron.lum_err / iron.lum GT 1.5) THEN BEGIN ;;CUSTOMIZABLE
           iron.quality += 4
        ENDIF
     ENDELSE
  ENDELSE
  out = CREATE_STRUCT(out, 'ironopt_br', iron)

  ;;Log QUALITY value
  IF (iron.quality NE 0) THEN BEGIN
     qsfit_log, 'Iron emission lines (Optical, broad):'
     FOR b=0, 2 DO BEGIN
        IF ((iron.quality AND 2b^b) GT 0) THEN $
           qsfit_log, '  ' + qsfit_iron_quality_meaning(b)
     ENDFOR
  ENDIF


  ;;Result structure
  iron = { lum:      gfit.comp.ironopt.norm_na.val, $
           lum_err:  gfit.comp.ironopt.norm_na.err, $
           fwhm:     gfit.comp.ironopt.fwhm_na.val, $
           fwhm_err: gfit.comp.ironopt.fwhm_na.err, $
           quality:  0b                             $
         }

  IF (~gfit.comp.ironopt.enabled) THEN BEGIN
     iron.lum      = gnan()
     iron.lum_err  = gnan()
     iron.fwhm     = gnan()
     iron.fwhm_err = gnan()
     iron.quality  = 1
  ENDIF $
  ELSE BEGIN
     IF (~FINITE(iron.lum)       OR    $
         ~FINITE(iron.lum_err)   OR    $
         (iron.lum     LE 0)     OR    $
         (iron.lum_err LE 0)     ) THEN BEGIN
        iron.quality = 2
     ENDIF $
     ELSE BEGIN
        IF (iron.lum_err / iron.lum GT 1.5) THEN BEGIN ;;CUSTOMIZABLE
           iron.quality += 4
        ENDIF
     ENDELSE
  ENDELSE
  out = CREATE_STRUCT(out, 'ironopt_na', iron)

  ;;Log QUALITY value
  IF (iron.quality NE 0) THEN BEGIN
     qsfit_log, 'Iron emission lines (Optical, narrow):'
     FOR b=0, 2 DO BEGIN
        IF ((iron.quality AND 2b^b) GT 0) THEN $
           qsfit_log, '  ' + qsfit_iron_quality_meaning(b)
     ENDFOR
  ENDIF


  ;;--------------------------
  ;;Reduce emission lines data
  lines = qsfit_lineset()

  alllines = []
  FOR j=0, gn(lines)-1 DO BEGIN
     IF (lines[j].type EQ 'N'  OR  lines[j].type EQ 'BN') THEN BEGIN
        tmp = qsfit_reduce_line('na_' + lines[j].name, lines[j].wave)
        out = CREATE_STRUCT(out, 'na_' + lines[j].name, tmp)
        alllines = [alllines, gstru_insert(tmp, 'line', 'na_' + lines[j].name, 0)]
     ENDIF

     IF (lines[j].type EQ 'B'  OR  lines[j].type EQ 'BN') THEN BEGIN
        tmp = qsfit_reduce_line('br_' + lines[j].name, lines[j].wave)
        out = CREATE_STRUCT(out, 'br_' + lines[j].name, tmp)
        alllines = [alllines, gstru_insert(tmp, 'line', 'br_' + lines[j].name, 0)]
     ENDIF
  ENDFOR

  ;;Save also all the lines results as an array
  out = CREATE_STRUCT(out, 'lines', alllines)

  RETURN, out
END


;=====================================================================
;NAME:
;
PRO qsfit_plot_current, filename
  COMMON GFIT
  gfit.plot.(0).main.title = ''
  rebin = gfit.plot.(0).main.rebin
  gfit.plot.(0).main.rebin = 5

  gfit_plot
  IF (gn(filename) EQ 1) THEN ggp, term='pdf', out=filename + '.pdf' $
  ELSE ggp

  gfit_plot_resid
  IF (gn(filename) EQ 1) THEN ggp, term='pdf', out=filename + '_resid.pdf' $
  ELSE ggp

  gfit.plot.(0).main.rebin = rebin
END

;=====================================================================
;NAME:
;  qsfit_run
;
;PURPOSE:
;  Calls QSFIT procedures in the appropriate sequence.
;
;PARAMETERS:
;  NONE.
;
PRO qsfit_run
  COMPILE_OPT IDL2
  ON_ERROR, !glib.on_error
  COMMON GFIT

  showSteps = 0

  ;;Avoid logging on iterations
  gfit.opt.log_iter = 0

  ;;Stop when relative difference in chi-squared is at most 1.e-6
  gfit.opt.tol = 1.e-6 ;;CUSTOMIZABLE

  ;;Fit continuum
  qsfit_add_continuum
  gfit_run
  IF (showSteps) THEN qsfit_plot_current, 'Step1'

  qsfit_renormalize_cont
  qsfit_freeze, cont=1
  IF (showSteps) THEN qsfit_plot_current, 'Step2'

  ;;Fit iron templates
  qsfit_add_iron
  gfit_run
  qsfit_freeze, iron=1
  IF (showSteps) THEN qsfit_plot_current, 'Step3'

  ;;Fit "known" emission lines
  qsfit_add_lineset
  gfit_run
  qsfit_freeze, lines=1
  IF (showSteps) THEN qsfit_plot_current, 'Step4'

  ;;Adding and fitting unknown lines can be very time-consuming.  In
  ;;order to save computation time we freeze all other line
  ;;parameters, and thaw them in the last fit.
  qsfit_freeze, lines=1, iron=1

  ;;Add "unknown" emission lines
  qsfit_add_unknown
  IF (showSteps) THEN qsfit_plot_current, 'Step5'

  ;;Run with all parameters freee
  gprint, 'Last run with all parameters free'
  qsfit_freeze, cont=0, iron=0, line=0
  gfit_run

  ;;Disable "unknown" lines whose normalization uncertainty is larger
  ;;than 3 times the normalization
  rerun = 0
  FOR iiunk=1, qsfit_nunklines() DO BEGIN
     iunk = WHERE(TAG_NAMES(gfit.comp) EQ 'UNK' + gn2s(iiunk))
     gassert, iunk NE -1
     IF (gfit.comp.(iunk).norm.err / gfit.comp.(iunk).norm.val GT 3) THEN BEGIN
        rerun = 1
        qsfit_log, 'Disabling line ' + gfit.comp.(iunk).name
        gfit.comp.(iunk).enabled = 0
     ENDIF
  ENDFOR

  ;;Re-run fitting if needed
  IF (rerun) THEN BEGIN
     qsfit_compile
     gfit_run
  ENDIF

  IF (showSteps) THEN qsfit_plot_current, 'Step6'
END



;=====================================================================
;NAME:
;  qsfit_report
;
;PURPOSE:
;  Print a report of GFIT results status.  Printing is performed
;  through gprint.
;
;PARAMETERS:
;  NONE.
;
PRO qsfit_report, red
  COMPILE_OPT IDL2
  ON_ERROR, !glib.on_error
  COMMON GFIT


  PRINT
  PRINT
  PRINT
  gfit_report, /all
  gprint

  gprint, 'Parameter covariance matrix (sorted by absolute value of covariance, down to covariance=0.5)'
  tmp = gfit_get_covar()
  tmp = tmp[WHERE(tmp.covar GT 0.5)]
  gps, tmp

  gprint
  gprint
  gprint, '====== QSFIT REPORT ======='
  gprint
  FOR i=0, gn(red.log)-1 DO $
     gprint, red.log[i]

  gprint
  gprint
  gprint, ' Continuum '
  gps, red.cont

  gprint
  gprint
  gprint, ' Host galaxy '
  gps, red.galaxy

  gprint
  gprint
  gprint, ' Iron emission lines (UV)'
  gprint, 'UV:'
  gps, [red.ironuv0, red.ironuv1, red.ironuv2, red.ironuv3]

  gprint
  gprint, ' Iron emission lines (optical, broad components)'
  gps, red.ironopt_br

  gprint
  gprint, ' Iron emission lines (optical, broad narrow)'
  gps, red.ironopt_na

  gprint
  gprint
  gprint, ' Emission lines '
  gps, red.lines

  gprint
  gprint
  gprint, ' Fit results '
  gps, gstru_sub(red.gfit.res, drop='covar')
END




;=====================================================================
;NAME:
;  qsfit_plot
;
;PURPOSE:
;  Produce plots using the GGP facility
;
;PARAMETERS:
;  RED (input, a scalar structure)
;    The RED structure as returned by the QSFIT function.
;
;  S11= (input, a scalar structure)
;    The Shen 2011 data, to compare the continuum luminosity and slope
;    results.
;
;  FILENAME= (optional input, a scalar string)
;    Path and file name prefix to save the gnuplot script files.
;
PRO qsfit_plot, red, FILENAME=filename, s11=s11
  COMPILE_OPT IDL2
  ON_ERROR, !glib.on_error

  gfit_restore, red.gfit

  ;;Prepare GFIT plot
  gfit_plot

  ;;Plot continuum luminosities and slopes from QSFIT
  ggp_data, red.cont.wave, red.cont.lum / red.cont.wave, red.cont.lum_err / red.cont.wave, $
            pl='w yerrorbars title "Cont. lum." pt 5 ps 1 lc rgb "red"'

  ;;FOR i=0, gn(red.cont)-1 DO BEGIN
  ;;   xx = red.cont[i].wave * [0.98, 1.02]
  ;;   yy = (xx / red.cont[i].wave)^red.cont[i].slope
  ;;   ggp_data, xx, (red.cont[i].lum / red.cont[i].wave)*yy, $
  ;;             pl='w lines notitle lw 1. lc rgb "red"'
  ;;ENDFOR

  ;;Plot continuum luminosities and slopes from S11
  IF (KEYWORD_SET(s11)) THEN BEGIN
     xx = [1350, 3000, 5100]
     yy = [s11.logl1350, s11.logl3000, s11.logl5100]
     ll = 10.d^(yy-42) / xx

     r0 = [1465, 2700, 4700, 6250]
     r1 = [1700, 2900, 5100, 6800]
     aa = [s11.alpha_civ, s11.alpha_mgii, s11.alpha_hb, s11.alpha_ha]

     PRINT, 'S11 lumin. : ', yy
     PRINT, 'S11 slopes.: ', aa

     i = WHERE(yy GT 20)
     IF (i[0] NE -1) THEN BEGIN
        IF (gn(i) EQ 1) THEN i = [i, i]
        ggp_data, xx[i], ll[i], pl='w points t "S11 lum." pt 7 ps 1 lc rgb "dark-green"'
     ENDIF

     i = WHERE(ABS(aa) GT 5)
     IF (i[0] NE -1) THEN aa[i] = gnan()
     ll = INTERPOL(red.cont[0:4].lum, red.cont[0:4].wave, r0) / r0

     xx = []
     yy = []
     r0 = FLOAT(r0)
     FOR i=0, gn(r0)-1 DO xx = [xx, r0[i], r1[i], gnan()]
     FOR i=0, gn(r0)-1 DO yy = [yy, ll[i], ll[i]*(r1[i]/r0[i])^aa[i], gnan()]

     ggp_data, xx, yy, pl='w lines t "S11 slopes" lw 3 lc rgb "dark-green"'
  ENDIF

  ;;Save or show plots
  IF (gn(filename) EQ 1) THEN ggp, gp=filename+'.gp' $
  ELSE ggp

  ;;Plot residuals
  gfit_plot_resid
  IF (gn(filename) EQ 1) THEN ggp, gp=filename+'_resid.gp' $
  ELSE ggp
END




;=====================================================================
;NAME:
;  qsfit_flatten_results
;
;PURPOSE:
;  Flatten the structure returned by the qsfit function into a single
;  level struture, suitable to be saved in a FITS table.
;
;PARAMETERS:
;  RED (input, a scalar structure)
;    The "reduced" structure as returned by the qsfit structure
;
;RETURN VALUE: (a scalar structure)
;  The flattened structure
;
FUNCTION qsfit_flatten_results, red
  out = red
  out = gstru_insert(out, 'chisq', red.gfit.res.test_stat)
  out = gstru_insert(out, 'dof'  , red.gfit.res.test_dof)
  out = gstru_insert(out, 'elapsed_time', red.gfit.res.elapsed_time)
  out = gstru_sub(out, drop=['file_output', 'gfit', 'expr', 'log', 'cont', 'lines'])
  out = gstru_flatten(out)
  RETURN, out
END


;=====================================================================
;NAME:
;  qsfit
;
;PURPOSE:
;  Main function to perform a spectral analysis on a SDSS (DR10)
;  spSpec file.
;
;PARAMETERS:
;  FILE (input, a scalar string)
;    Path to SDSS (DR10) spSpec file to be analyzed.
;
;  OUTDIR= (optional input, a scalar string)
;    Directory name where the output files will be saved.  If not
;    given no output file will be written.
;
;  PROCID= (optional input, a scalar integer)
;    The GFIT.opt.pid value.  If not given the default GFIT value is
;    used.  This number must be given when running multiple GFIT
;    instances using the same OUTDIR= keyword.
;
;  TICTOC= (keyword)
;    If given the TIC and TOC procedures are used to profile QSFIT
;    execution.
;
;  _EXTRA= (optional input)
;    Keywords passed to qsfit_prepare.
;
;RETURN VALUE:
;  The structure returned by qsfit_reduce().
;
FUNCTION qsfit, file, OUTDIR=outdir, PROCID=procid, TICTOC=tictoc, _EXTRA=extra
  COMPILE_OPT IDL2
  COMMON GFIT
  ON_ERROR, !glib.on_error

  ;;Ensure any previously opened file is closed
  gprint_mgr, /close
  gprint_mgr, use_stdout=1 ;;write on stabdard output

  ;;Catch errors to properly close the log file and return a NULL
  ;;value.
  IF (!glib.on_error EQ 2) THEN BEGIN
     CATCH, error
     IF (error NE 0) THEN BEGIN
        CATCH, /cancel
        gprint_error
        gprint_mgr, /close
        RETURN, []
     ENDIF
  ENDIF

  IF (~gfexists(file)) THEN $
     MESSAGE, 'Input file ' + file + ' do not exists!'

  IF (KEYWORD_SET(outdir)) THEN BEGIN
     fi = FILE_INFO(outdir)
     IF (~fi.exists  OR  ~fi.directory) THEN $
        MESSAGE, 'OUTDIR=' + outdir + ' do not exists or is not a directory'
     outName = outdir + '/' + FILE_BASENAME(file)
  ENDIF $
  ELSE $
     outName = file

  ;;Drop file extension
  tmp = STRPOS(outName, '.', /reverse_search)
  IF (tmp GE 0) THEN outName = STRMID(outName, 0, tmp)
  gassert, outName NE ''
  outName += '_QSFIT'
  file_dat = outName + '.dat'
  file_log = outName + '.log'

  ;;Check whether file_dat already exists
  IF (gfexists(file_dat)  AND  KEYWORD_SET(outdir))  THEN BEGIN
     gprint, 'File ' + file_dat + ' already exists'
     RESTORE, file_dat
     RETURN, qsfit_res
  ENDIF

  ;;Estimate elapsed time
  timeStart = SYSTIME(1)
  IF (KEYWORD_SET(tictoc)) THEN TIC, /profiler

  ;;Empty log queue
  qsfit_log, out=dummy
  dummy = []

  ;;Log input parameters
  gprint, '********************************************************************************'
  qsfit_log, 'QSFIT_SDSS10, ver. ' + qsfit_version()
  qsfit_log, '  started at ' + SYSTIME()
  qsfit_log
  qsfit_log, 'Input FILE   : ' + file
  IF (KEYWORD_SET(outidr)) THEN BEGIN
     qsfit_log, 'Output file  : ' + file_dat
     qsfit_log, 'Log file     : ' + file_log
  ENDIF
  qsfit_log

  IF (KEYWORD_SET(extra)) THEN BEGIN
     qsfit_log, 'Extra keywords:'
     gps, extra, row=0, out=tmp
     FOR i=0, gn(tmp)-1 DO $
        qsfit_log, tmp[i]
  ENDIF
  qsfit_log

  ;;Load data into GFIT
  qsfit_prepare, file, _EXTRA=extra

  ;;Set Process ID
  gfit.opt.pid = 0
  IF (gn(procid) EQ 1) THEN $
     gfit.opt.pid = LONG(procid[0])

  ;;Run QSFIT analysis and reduce results
  qsfit_run
  qsfit_res = qsfit_reduce()

  ;;Store elapsed time
  qsfit_res.gfit.res.elapsed_time = SYSTIME(1) - timeStart

  ;;Log results
  gprint
  gprint
  IF (KEYWORD_SET(outdir)) THEN gprint_mgr, file=file_log
  qsfit_report, qsfit_res
  gprint, 'Total elapsed time: ' + gn2s(qsfit_res.gfit.res.elapsed_time) + ' seconds.'
  gprint

  IF (KEYWORD_SET(outdir)) THEN BEGIN
     gprint_mgr, /close
     gprint, 'Log file: ' + file_log

     ;;Save results
     qsfit_log, 'Results saved in: ' + file_dat
     qsfit_res.file_output = outName
     SAVE, file=file_dat, qsfit_res, /compress
  ENDIF

  IF (KEYWORD_SET(tictoc)) THEN BEGIN
     TOC, REPORT=d
     d=d[SORT(d.ONLY_TIME)]
     gps, d
     PRINT & PRINT & PRINT
     d=d[SORT(d.name)]
     gps, d
  ENDIF

  RETURN, qsfit_res
END
