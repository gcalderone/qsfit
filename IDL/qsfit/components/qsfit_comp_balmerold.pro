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


PRO qsfit_comp_balmer_hol_compute, $
   Temp, $
   logDensity, $
   fwhm, $
   x, hol ;; OUTPUT

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
  dummy = MIN(ABS(logDensity - logNeAvail), iNe)

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
  hol /= INT_TABULATED(x, hol)
END




PRO qsfit_comp_balmer_hol_prepare, hol_x, hol
  path = FILE_DIRNAME(ROUTINE_FILEPATH('qsfit_comp_balmer_hol_prepare')) + PATH_SEP()
  IF (gfexists(path + 'qsfit_comp_balmer_hol.dat')) THEN BEGIN
     RESTORE,  path + 'qsfit_comp_balmer_hol.dat'
     RETURN
  ENDIF

  logT   = ALOG10([5, 10, 30, 50, 75, 100, 125, 150, 200, 300] * 100.)
  logNe  = [2, 3, 4, 5, 6, 7, 8, 9, 10, 11, 12, 13, 14]
  fwhm   = ggen(1e3, 1e4, 50)

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




PRO qsfit_comp_balmer_hol_init, comp
  COMPILE_OPT IDL2
  ON_ERROR, !glib.on_error
  COMMON COM_qsfit_comp_balmer_hol, hol_x, hol, curr

  firstTime = 0b
  IF (gn(hol) EQ 0) THEN $
     firstTime = 1b

  IF (firstTime) THEN $
     qsfit_comp_balmer_hol_prepare, hol_x, hol

  comp.norm.val       = 1
  comp.norm.limits[0] = 0

  comp.logT.val       = ALOG10(15000.)
  comp.logT.fixed     = 1

  comp.logNe.val      = 9
  comp.logNe.fixed    = 1 ;this is degenerate with component normalization, hence it should be fixed

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
  ENDIF

  curr = []
END



FUNCTION qsfit_comp_balmer_hol, x, norm, logT, logNe, fwhm
  COMPILE_OPT IDL2
  ON_ERROR, !glib.on_error
  COMMON COM_qsfit_comp_balmer_hol

  IF (gn(curr) EQ 0) THEN BEGIN
     PRINT, 'Interpolation of high order Balmer lines template...'
     curr = FLTARR(gn(x), gn(hol))
     FOR i=0, gn(hol)-1 DO $
        curr[*,i] = INTERPOL(hol[i].y, hol_x, x)
  ENDIF

  ;;Find best template
  dummy = MIN(ABS(hol.logT - logT)     , i)  &   i = WHERE(hol.logT     EQ hol[i].logT)
  dummy = MIN(ABS(hol[i].logNe - logNe), j)  &   j = WHERE(hol[i].logNe EQ hol[i[j]].logNe)  &   i = i[j]
  dummy = MIN(ABS(hol[i].fwhm  - fwhm) , j)  &   j = WHERE(hol[i].fwhm  EQ hol[i[j]].fwhm)   &   i = i[j]
  gassert, gn(i) EQ 1

  RETURN, norm * curr[*,i]
END



PRO qsfit_comp_balmer_cont_init, comp
  COMPILE_OPT IDL2
  ON_ERROR, !glib.on_error

  comp.norm.val       = 1
  comp.norm.limits[0] = 0

  comp.logT.val       = ALOG10(15000.)
  comp.logT.limits    = [3.5, 4.2]
  comp.logT.fixed     = 1
  comp.logT.step      = 0.05

  ;;In the raneg 0..1 this parameter provide the most sensible changes
  ;;in the range 2000-3645.A.  At logTau<0 this parameter become
  ;;completely degenerate with normalization.  At logTau>1 the black
  ;;body spectrum is unaffected in the range 2000-3645.
  comp.logTau.val     = 0
  comp.logTau.limits  = [0, 1]
  comp.logTau.fixed   = 1 ;;highly correlated with normalization
  comp.logT.step      = 0.2

  comp.fwhm.val       = 5000.
  comp.fwhm.limits    = [1e2, 3e4]
  comp.fwhm.fixed     = 1 ;;this is not a sensible parameter hence its value should be fixed.

  curr = []
END



;; Calculation of Balmer continuum (Grandi 1982, Apj 255, 25G and
;; Dietrich et al. 2002, Apj, 564, 581)
FUNCTION qsfit_comp_balmer_cont, x, norm, logT, logTau, fwhm
  COMPILE_OPT IDL2
  ON_ERROR, !glib.on_error

  c = gpc()
  ryd = c.r_inf/c.ev                 ;; eV
  ryd = c.c / (c.r_inf / c.h) * 1.d8 ;; Angstrom
  edge = ryd*4.                      ;; Balmer edge

  Temp = 10.^logT
  tau  = 10.^logTau

  ;; Simple approach, but very slow
  IF (0) THEN BEGIN
     x1 = ggen(912, 4000, 10000) ;; Angstrom
     l = x1*1.e-8                ;; wavelengths in cm

     ;; Planck function
     b = 2 * c.h * c.c^2.d / (l^5.d)
     exp = DOUBLE(c.h*c.c / (l * c.k * Temp))
     IF (gsearch(exp LT 80, i)) THEN $
        b[i] /= (EXP(exp[i]) - 1.d)
     IF (gsearch(exp GE 80, i)) THEN $
        b[i] /= EXP(exp[i])

     ;; Take into account optical depth
     bac1 = b * (1.d - EXP(-(tau * (x1/edge)^3.d)))
     bac1[WHERE(x1 GE edge)] = 0.
     bac1 /= MAX(bac1)

     ;; Broadening
     IF (~KEYWORD_SET(fwhm)) THEN fwhm = 5000. ;km/s
     IF (FWHM GT 1) THEN BEGIN
        tmp = bac1*0.d
        FOR i=0, gn(x1)-1 DO BEGIN
           s = (fwhm / 3.e5) * x1[i] / 2.35
           exp = ((x1-x1[i]) / s)^2. / 2.
           tmp += bac1[i] * EXP( -exp ) / s
        ENDFOR
        bac1 = TEMPORARY(tmp)
     ENDIF
  ENDIF


  ;; Best approach
  xx = gloggen(912, 4000, 500) ;; Angstrom
  l = xx*1.e-8 ;;  wavelengths in cm

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

  ;x = gloggen(0.01, 3, 100)
  ;ggp_clear
  ;ggp_cmd, /xlog, /ylog, xr=gminmax(x), yr=[1.e-4, 2]
  ;ggp_cmd, xr=[0.60, 2], yr=[0.1, 1]
  ;ggp_data, x, 1-EXP(-(0.1*(x^3.))), pl='w l t "0.1"'
  ;ggp_data, x, 1-EXP(-(1  *(x^3.))), pl='w l t "1"'
  ;ggp_data, x, 1-EXP(-(3  *(x^3.))), pl='w l t "3"'
  ;ggp_data, x, 1-EXP(-(10 *(x^3.))), pl='w l t "10"'
  ;ggp_data, x, 1-EXP(-(100*(x^3.))), pl='w l t "100"'
  ;ggp_data, x, 1.e-4*(x/0.1)^3     , pl='w l dt 2 notit'
  ;ggp_data, x, 1.e-3*(x/0.1)^3     , pl='w l dt 2 notit'
  ;ggp_data, x, 1.e-2*(x/0.1)^3     , pl='w l dt 2 notit'
  ;ggp_data, x, 1.e-1*(x/0.1)^3     , pl='w l dt 2 notit'
  ;ggp


  ;;Broadening
  s = (fwhm / 3.e5) * ((xx[1]+xx[0])/2.) / 2.35 / (xx[1]-xx[0])
  ;help, s
  bac = CONVOL(bac, GAUSSIAN_FUNCTION(s, width=s*6), /edge_truncate, /center)

  ;;Compare approaches
  IF (0) THEN BEGIN
     bac1/= MAX(bac1)
     bac /= MAX(bac)

     ggp_clear
     ggp_data, x1, bac1, pl='w l'
     ggp_data, xx, bac , pl='w l'
     ggp
  ENDIF

  IF (0) THEN BEGIN
     dummy = qsfit_comp_balmer_cont(0, 1., 4.5,  0., 3e4)
     dummy = qsfit_comp_balmer_cont(0, 1., 2.5,  0., 3e4)
     dummy = qsfit_comp_balmer_cont(0, 1., 4.5,  0., 1e2)
     dummy = qsfit_comp_balmer_cont(0, 1., 2.5,  0., 1e2)
     dummy = qsfit_comp_balmer_cont(0, 1., 4.5, -2., 3e4)
     dummy = qsfit_comp_balmer_cont(0, 1., 2.5, -2., 3e4)
     dummy = qsfit_comp_balmer_cont(0, 1., 4.5, -2., 1e2)
     dummy = qsfit_comp_balmer_cont(0, 1., 2.5, -2., 1e2)
     dummy = qsfit_comp_balmer_cont(0, 1., 4.5,  2., 3e4)
     dummy = qsfit_comp_balmer_cont(0, 1., 2.5,  2., 3e4)
     dummy = qsfit_comp_balmer_cont(0, 1., 4.5,  2., 1e2)
     dummy = qsfit_comp_balmer_cont(0, 1., 2.5,  2., 1e2)
  ENDIF

  ;; Normalize Balmer continuum at 3000A
  bac /= INTERPOL(bac, xx, 3600) * 3600.
  RETURN, norm * INTERPOL(bac, xx, x)
END





PRO qsfit_comp_balmer_aa
  qsfit_comp_balmer_hol_prepare, hol_x, hol
  comp = gfit_component('qsfit_comp_balmer_hol')
  comp = gfit_component('qsfit_comp_balmer_cont')


  x = ggen(2000, 3800, 1000)
  ggp_clear
  ggp_cmd, 'set key left', $
           xtit='Wavelength [A]', ytit='Lum. density [arb.units]'

  norm = 3400.
  y1 = qsfit_comp_balmer_cont(x, norm, 3.5 , 0, 5000)
  y2 = qsfit_comp_balmer_cont(x, norm, 3.7 , 0, 5000)
  y3 = qsfit_comp_balmer_cont(x, norm, 4.18, 0, 5000)
  ggp_data, x, y1, pl='w l tit "logT=3.5" lw 2 lc rgb "red"'
  ggp_data, x, y2, pl='w l tit "logT=3.7" lw 2 lc rgb "blue"'
  ggp_data, x, y3, pl='w l tit "logT=4.18" lw 2 lc rgb "dark-green"'

  y1 = qsfit_comp_balmer_cont(x, norm, 3.5 ,-1, 5000)
  y2 = qsfit_comp_balmer_cont(x, norm, 3.7 ,-1, 5000)
  y3 = qsfit_comp_balmer_cont(x, norm, 4.18,-1, 5000)
  ggp_data, x, y1, pl='w l notit dt 3 lw 2 lc rgb "red"'
  ggp_data, x, y2, pl='w l notit dt 3 lw 2 lc rgb "blue"'
  ggp_data, x, y3, pl='w l notit dt 3 lw 2 lc rgb "dark-green"'

  y1 = qsfit_comp_balmer_cont(x, norm, 3.5 ,0.3, 5000)
  y2 = qsfit_comp_balmer_cont(x, norm, 3.7 ,0.3, 5000)
  y3 = qsfit_comp_balmer_cont(x, norm, 4.18,0.3, 5000)
  ggp_data, x, y1, pl='w l notit dt 2 lw 2 lc rgb "red"'
  ggp_data, x, y2, pl='w l notit dt 2 lw 2 lc rgb "blue"'
  ggp_data, x, y3, pl='w l notit dt 2 lw 2 lc rgb "dark-green"'

  ggp








  gdist, hol.logNe, d
  logNe = d[FLOOR(gn(d)/2.)]
  gdist, hol.fwhm, d
  fwhm = d[0];d[FLOOR(gn(d)/2.)]
  gdist, hol.logT, d
  ;logT = d[FLOOR(gn(d)/2.)]
  FOR i=0, gn(d)-1 DO BEGIN
     y1 = qsfit_comp_balmer_hol(hol_x, 1, d[i], logNe, fwhm)
     qsfit_comp_balmer_hol_compute, 10.^d[i], logNe, fwhm, x, y2
     PLOT, hol_X, y1, yr=gminmax(hol.y)>1.e-8, /ylog
     OPLOT, x, y2, col=255, psym=3
     PRINT, 'logT ', d[i]
     gkey
  ENDFOR


  gdist, hol.logT, d
  logT = d[FLOOR(gn(d)/2.)]
  gdist, hol.logNe, d
  logNe = d[FLOOR(gn(d)/2.)]
  gdist, hol.fwhm, d
  ;fwhm = d[FLOOR(gn(d)/2.)]
  FOR i=0, gn(d)-1 DO BEGIN
     y1 = qsfit_comp_balmer_hol(hol_x, 1, logT, logNe, d[i])
     qsfit_comp_balmer_hol_compute, 10.^logT, logNe, d[i], x, y2
     PLOT, hol_X, y1, yr=gminmax(hol.y)>1.e-8, /ylog
     OPLOT, x, y2, col=255, psym=3
     PRINT, 'fwhm ', d[i]
     gkey
  ENDFOR


  gdist, hol.logT, d
  logT = d[FLOOR(gn(d)/2.)]
  gdist, hol.fwhm, d
  fwhm = d[0];d[FLOOR(gn(d)/2.)]
  gdist, hol.logNe, d
  ;logNe = d[FLOOR(gn(d)/2.)]
  FOR i=0, gn(d)-1 DO BEGIN
     y1 = qsfit_comp_balmer_hol(hol_x, 1, logT, d[i], fwhm)
     qsfit_comp_balmer_hol_compute, 10.^logT, d[i], fwhm, x, y2
     PLOT, hol_X, y1, yr=gminmax(hol.y)>1.e-8, /ylog
     OPLOT, x, y2, col=255, psym=3
     PRINT, 'logNe ', d[i]
     gkey
  ENDFOR
END
