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
;  qsfit_comp_balmer_hol_compute
;
;PURPOSE:
;  Calculate the Balmer pseudo-continuum spectral profile due to the
;  super-position of high order (7->2, and higher) Balmer
;  emission lines.
;
;  The calculation is performed using the list of relative line
;  intensities given in Storey and Hummer (1995) under the case B
;  approximation for Hydrogen, for a given temperature, electron
;  density, and braodening Doppler velocity.
;
;  The list of available temperature and electron densities are given
;  in the code (in the Tavail and logNeAvail arrays respectively).  If
;  the requested temperature or density values are not available the
;  closest available ones are used.
;
;  The spectral profile broadening is performed by convolving the
;  original spectrum with a Gaussian profile of a given FWHM.
;
;PARAMETERS:
;  Temp: (input, a number)
;    The electron temperature, in Kelvin.
;
;  logNe: (input, a number)
;    The electron density, in log10(cm^-3).
;
;  fwhm: (input, a number)
;    The FWHM of the Gaussian profile to broaden the template, in
;    km/s.
;
;  x: (output, array of numbers)
;    The wavelength (in Angstrom) at which the template is evaluated.
;
;  hol: (output, array of numbers)
;    The luminosity density of the spectral profile, in arbitrary
;    units, normalized at the Balmer edge wavelength (3645.07
;    Angstrom).
;
;NOTES:
;  Reference paper: Storey and Hummer 1995
;  (http://adsabs.harvard.edu/abs/1995MNRAS.272...41S)
;
;  Data files downloaded from
;  http://cdsarc.u-strasbg.fr/ftp/cats/VI/64/
;
PRO qsfit_comp_balmer_hol_compute, Temp, logNe, fwhm, x, hol
  c = gpc()
  ryd = c.r_inf/c.ev                 ;; eV
  ryd = c.c / (c.r_inf / c.h) * 1.d8 ;; Angstrom
  edge = ryd*4.                      ;; Balmer edge

  path = FILE_DIRNAME(ROUTINE_FILEPATH('qsfit_comp_balmer_hol_compute')) + PATH_SEP()
  path += 'SH1995' + PATH_SEP()

  ;; List of available temperatures in SH95 [K/100]
  Tavail = [5, 10, 30, 50, 75, 100, 125, 150, 200, 300]
  dummy = MIN(ABS(Temp/100. - Tavail), iTemp)

  ;; List of available electron densities in SH95 [log (Ne / cm^-3)]
  logNeAvail = [2, 3, 4, 5, 6, 7, 8, 9, 10, 11, 12, 13, 14]
  dummy = MIN(ABS(logNe - logNeAvail), iNe)

  ;; Read appropriate file
  file = STRING(format=gcfmt(path + 'r1b%04d.d'), Tavail[iTemp])
  electronDensity = STRING(format=gcfmt('1.000E+%02d'), logNeAvail[iNe])
  ;PRINT, 'Reading file ', file, ' with density ', electronDensity
  tmp = "cat " + file + " | " + path + "convert.pl | grep E_NU | grep 'NE= " + electronDensity + "' | perl -pe 's/ *Z=.*CASE=B//g; s/E_NU=//g' > " + path + "tmp"
  ;PRINT, tmp
  SPAWN, tmp

  wave = LIST()
  line = LIST()
  OPENR, lun, path + "tmp", /get_lun
  WHILE (~EOF(lun)) DO BEGIN
     l = ''
     READF, lun, l
     l = STRSPLIT(l, ' ', /extract)
     upper = LONG(l[0])

     FOR lower=1, upper-1 DO BEGIN
        IF (lower EQ 1) THEN CONTINUE                  ;; Skip Lyman lines
        IF (lower EQ 2  AND  upper EQ 3) THEN CONTINUE ;; Skip Ha line
        IF (lower EQ 2  AND  upper EQ 4) THEN CONTINUE ;; Skip Hb line
        IF (lower EQ 2  AND  upper EQ 5) THEN CONTINUE ;; Skip Hg line
        IF (lower EQ 2  AND  upper EQ 6) THEN CONTINUE ;; Skip Hd line
        ;;IF (lower EQ 2  AND  upper EQ 7) THEN CONTINUE
        ;;IF (lower EQ 2  AND  upper EQ 8) THEN CONTINUE
        ;;IF (lower EQ 2  AND  upper EQ 9) THEN CONTINUE

        wl = ryd / (1.d/lower^2.d - 1.d/upper^2.d)
        IF (wl GT 4200) THEN CONTINUE

        ;; gprint, lower, upper, FLOAT(l[lower]), ryd / (1.d/lower^2.d - 1.d/upper^2.d)
        wave.add, wl
        line.add, FLOAT(l[lower])
     ENDFOR
  ENDWHILE
  FREE_LUN, lun
  FILE_DELETE, path + "tmp"

  wave = wave.toArray()
  line = line.toArray()
  line /= MAX(line)

  ;; Prepare output variables
  x = gloggen(3400, 4300, 500) ;; Angstrom
  hol = DOUBLE(x*0.)           ;; Lum. density (arbitrary units)

  ;; Broadening of high order Balmer lines ==> psuedo-continuum
  FOR i=0, gn(wave)-1 DO BEGIN
     s = (fwhm / 3.e5) * wave[i] / 2.35
     exp = ((x-wave[i]) / s)^2. / 2.
     hol += line[i] * EXP( -exp ) / s
  ENDFOR

  ;; Normalize blending of high order Balmer lines
  hol /= INTERPOL(hol, x, edge)
END




;=====================================================================
;NAME:
;  qsfit_comp_balmer_hol_prepare
;
;PURPOSE:
;  Pre-calculate several spectral profile of the Balmer
;  pseudo-continuum profile (using qsfit_comp_balmer_hol_compute) and
;  save them in a IDL file to be quickly read and used during qsfit
;  operations.
;
;PARAMETERS:
;  hol_x: (output, array of numbers)
;    The wavelength (in Angstrom) at which the templates are evaluated.
;
;  hol: (output, array of structures)
;    Each stucture has the following tags:
;    - logT: the logarithm of the electron temperature (in Kelvin);
;    - logNe: the logarithm of the electron density (in cm^-3);
;    - fwhm: the FWHM of the Gaussian profile used to broaden the
;      template (in km/s);
;    - Y: the luminosity density of the spectral profile, in arbitrary
;      units, normalized at the Balmer edge wavelength (3645.07
;      Angstrom).
;
PRO qsfit_comp_balmer_hol_prepare, hol_x, hol
  path = FILE_DIRNAME(ROUTINE_FILEPATH('qsfit_comp_balmer_hol_prepare')) + PATH_SEP()
  IF (gfexists(path + 'qsfit_comp_balmer_hol.dat')) THEN BEGIN
     RESTORE,  path + 'qsfit_comp_balmer_hol.dat'
     RETURN
  ENDIF

  logT   = ALOG10([5, 10, 30, 50, 75, 100, 125, 150, 200, 300] * 100.)
  logNe  = [2, 3, 4, 5, 6, 7, 8, 9, 10, 11, 12, 13, 14]
  fwhm   = 5000.                ;ggen(1e3, 1e4, 50)

  qsfit_comp_balmer_hol_compute, 10.^logT[0], logNe[0], fwhm[0], hol_x, hol_y
  hol = REPLICATE({ $
        logT: logT[0]   , $
        logNe: logNe[0] , $
        fwhm: fwhm[0]   , $
        y: hol_y          $
  }, gn(logT) * gn(logNe) * gn(fwhm))

  i = 0
  FOR i0=0, gn(logT)-1 DO $
     FOR i1=0, gn(logNe)-1 DO $
        FOR i2=0, gn(fwhm)-1 DO $
           BEGIN
     PRINT, i, gn(hol)
     qsfit_comp_balmer_hol_compute, 10.^logT[i0], logNe[i1], fwhm[i2], hol_x, hol_y
     hol[i].logT  = logT[i0]
     hol[i].logNe = logNe[i1]
     hol[i].fwhm  = fwhm[i2]
     hol[i].y     = hol_y
     i += 1
     ;plot, hol_x, hol_y
  ENDFOR

  SAVE, file=path + 'qsfit_comp_balmer_hol.dat', hol_x, hol, /compress
END



;=====================================================================
;GFIT MODEL COMPONENT
;
;NAME:
;  qsfit_comp_balmer
;
;COMPONENT DESCRIPTION:
;  This template provide the spectral profile of the Hydrogen Balmer
;  continuum (at wavelengths shorter than the Balmer edge at 3645A)
;  and of the pseudo-continuum given by the super position of high
;  order Balmer emission lines (at wavelengths longer than the Balmer
;  edge).  The model is described in Grandi 1982
;  (http://adsabs.harvard.edu/abs/1982ApJ...255...25G) and Dietrich et
;  al. 2002 (http://adsabs.harvard.edu/abs/2002ApJ...564..581D).
;
;  This component can only be used in conjunction with
;  qsfit_comp_sbpowerlaw to account for the AGN continuum, since the
;  normalization of the former is expressed as a fraction of the
;  lumiosity of the latter.
;
;PARAMETERS:
;  NORM (no units)
;    Luminosity density of the Balmer template at 3000A, in units of
;    the qsfit_comp_sbpowerlaw component at the same wavelength.
;
;  RATIO (no units)
;    Ratio of Balmer continuum to pseudo-continuum luminosity at the
;    Balmer edge (3645A).
;
;  LOGT (log Kelvin)
;    Logarithm (base 10) of the electron temperature.
;
;  LOGNE (log cm^-3)
;    Logarithm (base 10) of the electron density.
;
;  LOGTAU (no units)
;    Logarithm (base 10) of the optical depth at the Balmer edge.
;
;  FWHM (km/s)
;    FWHM of the gaussian profile used to broaden the template.
;
;OPTIONS:
;  NONE
;
PRO qsfit_comp_balmer_init, comp
  COMPILE_OPT IDL2
  ON_ERROR, !glib.on_error
  COMMON COM_qsfit_comp_balmer, hol_x, hol, curr, save_bac, bac, bac_at_edge

  firstTime = 0b
  IF (gn(hol) EQ 0) THEN $
     firstTime = 1b

  IF (firstTime) THEN $
     qsfit_comp_balmer_hol_prepare, hol_x, hol

  comp.norm.val       = 0.1
  comp.norm.limits    = [0, 0.5]

  comp.ratio.val      = 1
  comp.ratio.limits   = [0.3, 1]

  comp.logT.val       = ALOG10(15000.)
  comp.logT.fixed     = 1

  comp.logNe.val      = 9
  comp.logNe.fixed    = 1 ;this is degenerate with normalization, hence it should be fixed

  ;;In the range 0..1 this parameter provide the most sensible changes
  ;;in the range 2000-3645.A.  At logTau<0 this parameter become
  ;;completely degenerate with normalization.  At logTau>1 the black
  ;;body spectrum is unaffected in the range 2000-3645.
  comp.logTau.val     = 0
  comp.logTau.limits  = [0, 1]
  comp.logTau.fixed   = 1 ;;highly correlated with normalization
  comp.logT.step      = 0.1

  comp.fwhm.val       = 5040.8
  comp.fwhm.fixed     = 1

  IF (firstTime) THEN BEGIN
     ;; To allow significant performance improvement drop data in
     ;; templates corresponding to unused values of logT/logNe/fwhm.
     IF (comp.logT.fixed) THEN BEGIN
        dummy = MIN(ABS(hol.logT - comp.logT.val), i)
        i = WHERE(hol.logT EQ hol[i[0]].logT)
        hol = hol[i]
     ENDIF $
     ELSE BEGIN
        gdist, hol.logT, tmp
        comp.logT.step   = tmp[1] - tmp[0]
        comp.logT.limits = gminmax(hol.logT)
     ENDELSE

     IF (comp.logNe.fixed) THEN BEGIN
        dummy = MIN(ABS(hol.logNe - comp.logNe.val), i)
        i = WHERE(hol.logNe EQ hol[i[0]].logNe)
        hol = hol[i]
     ENDIF $
     ELSE BEGIN
        gdist, hol.logNe, tmp
        comp.logNe.step   = tmp[1] - tmp[0]
        comp.logNe.limits = gminmax(hol.logNe)
     ENDELSE

     IF (comp.fwhm.fixed) THEN BEGIN
        dummy = MIN(ABS(hol.fwhm - comp.fwhm.val), i)
        i = WHERE(hol.fwhm EQ hol[i[0]].fwhm)
        hol = hol[i]
     ENDIF $
     ELSE BEGIN
        gdist, hol.fwhm, tmp
        comp.fwhm.step   = tmp[1] - tmp[0]
        comp.fwhm.limits = gminmax(hol.fwhm)
     ENDELSE

     save_bac = 0b
     IF (comp.logT.fixed    AND  $
         comp.logTau.fixed  AND  $
         comp.fwhm.fixed)   THEN $
            save_bac = 1b
  ENDIF

  curr = []
  bac = []
END



FUNCTION qsfit_comp_balmer, x, norm, ratio, logT, logNe, logTau, fwhm
  COMPILE_OPT IDL2
  ON_ERROR, !glib.on_error
  COMMON COM_qsfit_comp_balmer

  IF (gn(curr) EQ 0) THEN BEGIN
     PRINT, 'Interpolation of high order Balmer lines template...'
     curr = FLTARR(gn(x), gn(hol))
     FOR i=0, gn(hol)-1 DO $
        curr[*,i] = INTERPOL(hol[i].y, hol_x, x)
  ENDIF

  ;;Find best HOL template
  dummy = MIN(ABS(hol.logT - logT)     , i)  &   i = WHERE(hol.logT     EQ hol[i].logT)
  dummy = MIN(ABS(hol[i].logNe - logNe), j)  &   j = WHERE(hol[i].logNe EQ hol[i[j]].logNe)  &   i = i[j]
  dummy = MIN(ABS(hol[i].fwhm  - fwhm) , j)  &   j = WHERE(hol[i].fwhm  EQ hol[i[j]].fwhm)   &   i = i[j]
  gassert, gn(i) EQ 1
  iHOL = TEMPORARY(i)

  IF ((gn(bac) EQ 0)  OR  ~save_bac) THEN BEGIN
     c = gpc()
     ryd = c.r_inf/c.ev                 ;; eV
     ryd = c.c / (c.r_inf / c.h) * 1.d8 ;; Angstrom
     edge = ryd*4.                      ;; Balmer edge

     Temp = 10.^logT
     tau  = 10.^logTau

     xx = gloggen(912, 4000, 500) ;; Angstrom
     l = xx*1.e-8                 ;; wavelengths in cm

     ;; Planck function
     b = 2 * c.h * c.c^2.d / (l^5.d)
     exp = DOUBLE(c.h*c.c / (l * c.k * Temp))
     IF (gsearch(exp LT 80, i)) THEN $
        b[i] /= (EXP(exp[i]) - 1.d)
     IF (gsearch(exp GE 80, i)) THEN $
        b[i] /= EXP(exp[i])

     ;; Take into account optical depth
     bac = b * (1.d - EXP(-(tau * (xx/edge)^3.d)))
     bac[WHERE(xx GE edge)] = 0.
     bac /= MAX(bac)

     ;;Broadening
     s = (fwhm / 3.e5) * ((xx[1]+xx[0])/2.) / 2.35 / (xx[1]-xx[0])
     bac = CONVOL(bac, GAUSSIAN_FUNCTION(s, width=s*6), /edge_truncate, /center)

     ;; Normalize Balmer continuum at 3000A
     bac /= INTERPOL(bac, xx, 3000.)
     bac_at_edge = INTERPOL(bac, xx, edge)
     bac = INTERPOL(bac, xx, x)
  ENDIF

  ;;PLOT , x, bac
  ;;OPLOT, x, ratio * bac_at_edge * curr[*,iHOL], col=255

  RETURN, norm * qsfit_comp_sbpowerlaw_l3000() * (bac + ratio * bac_at_edge * curr[*,iHOL])
END





PRO qsfit_comp_balmer_test
  qsfit_comp_balmer_hol_prepare, hol_x, hol
  comp = gfit_component('qsfit_comp_sbpowerlaw')
  comp = gfit_component('qsfit_comp_balmer')

  x = ggen(1000, 4100, 1000)
  ggp_clear
  ggp_cmd, 'set key left', $
           xtit='Wavelength [A]', ytit='Lum. density [arb.units]'

  y1 = qsfit_comp_balmer(x, 1, 1  , ALOG10(15000), 9, 0, 5000)
  y2 = qsfit_comp_balmer(x, 1, 0.5, ALOG10(15000), 9, 0, 5000)
  ggp_data, x, y1, pl='w l tit "ratio=1" lw 2 lc rgb "red"'
  ggp_data, x, y2, pl='w l tit "ratio=0.5" lw 2 lc rgb "blue"'
  ggp_data, 3645.07*[1,1], [0, 1.4], pl='w l tit "Balmer edge" dt 4 lc rgb "black"'
  ;ggp, term='pdf fontscale 0.65 linewidth 1.3', out='qsfit_comp_balmer_test.pdf'
  ggp, term='pdf', out='qsfit_comp_balmer_test.pdf'
END
