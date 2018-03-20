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
;   qsfit_comp_galaxytemplate
;
;COMPONENT DESCRIPTION:
;  A galaxy template
;
;PARAMETERS:
;  NORM
;    The galaxy template normalization.  The units depends on the
;    chosen template, see the ID option.
;
;OPTIONS:
;  ID: (a scalar string)
;    The template to be used. Valid values are:
;      M01_E : Mannucci et al. 2001, E template
;      M01_S0: Mannucci et al. 2001, S0 template
;      M01_SA: Mannucci et al. 2001, SA template
;      M01_SB: Mannucci et al. 2001, SB template
;      M01_SC: Mannucci et al. 2001, SC template
;
;  When the M01_* templates are used the NORM parameter is the
;  bolometric luminosity of the templates in units of X*Y.
;
PRO qsfit_comp_galaxytemplate_init, comp
  COMPILE_OPT IDL2
  ON_ERROR, !glib.on_error

  comp.par.norm.val = 1
  comp.par.norm.limits = [1.e-2, 10]
END


FUNCTION qsfit_comp_galaxytemplate_opt, comp
  ;;Default value for the template_id option
  ;;Templates from Mannucci et al. 2001
  ;; 'M01_E'
  ;; 'M01_S0'
  ;; 'M01_SA'
  ;; 'M01_SB'
  ;; 'M01_SC'

  ;;SWIRE galaxy templates:
  ;; 'SWIRE_ELL2'   ;;2 Gyr old elliptical (Silva et al. 1998, ApJ, 509, 103)
  ;; 'SWIRE_ELL5'   ;;5 Gyr old elliptical
  ;; 'SWIRE_ELL13'  ;;13 Gyr old elliptical
  ;; 'SWIRE_S0'     ;;Spiral 0 (PAH modified using IRS spectra of normal galaxies)
  ;; 'SWIRE_SA'     ;;Spiral a (PAH modified using IRS spectra of normal galaxies)
  ;; 'SWIRE_SB'     ;;Spiral b (PAH modified using IRS spectra of normal galaxies)
  ;; 'SWIRE_SC'     ;;Spiral c (PAH modified using IRS spectra of normal galaxies)
  ;; 'SWIRE_SDM'    ;;Spiral dm (PAH modified using IRS spectra of normal galaxies)
  ;; 'SWIRE_SPI4'   ;;Spiral c (PAH modified using IRS spectra of normal galaxies)
  ;; 'SWIRE_M82'    ;;Starburst galaxy M82 (PAH modified using ISO spectrum)
  ;; 'SWIRE_N6090'  ;;Starburst galaxy NGC 6090 (PAH modified using ISO spectrum)
  ;; 'SWIRE_ARP220' ;;Starburst galaxy Arp 220 (ULIRG) (PAH modified using IRS spectrum)

  ;; 'SWIRE_I20551'
  ;; 'SWIRE_I22491'
  ;; 'SWIRE_MRK231'
  ;; 'SWIRE_BQSO1'

  ;; 'SWIRE_I19254'
  ;; 'SWIRE_SEY18'
  ;; 'SWIRE_TORUS'
  ;; 'SWIRE_TQSO1'
  ;; 'SWIRE_N6240'
  ;; 'SWIRE_SEY2'
  ;; 'SWIRE_SD'
  ;; 'SWIRE_QSO1'
  ;; 'SWIRE_QSO2'
  opt = {template_id: 'M01_E'}
  RETURN, opt
END


FUNCTION qsfit_comp_galaxytemplate_cdata, comp, x, cdata
  COMPILE_OPT IDL2
  ON_ERROR, !glib.on_error
  COMMON COM_qsfit_comp_galaxytemplate, m01, swire

  IF (gn(cdata) GT 0) THEN RETURN, cdata

  ;;Read templates
  IF (gn(m01) EQ 0) THEN BEGIN
     m01 = ggaltempl_mannucci01()
     ;;FOR i=2, N_TAGS(m01)-1 DO $      ;;normalize templates
     ;;   m01.(i) /= INT_TABULATED(m01.lambda, m01.(i))

     swire = ggaltempl_swire()
  ENDIF

  ;;Initialize template
  qsfit_log, 'Using galaxy template: ' + comp.opt.template_id

  IF (STRMID(comp.opt.template_id, 0, 3) EQ 'M01') THEN BEGIN
     i = WHERE(TAG_NAMES(m01) EQ STRUPCASE(STRMID(comp.opt.template_id, 4)))
     IF (i[0] EQ -1) THEN $
        MESSAGE, 'Uknown galaxy template: ' + comp.opt.template_id
     template_wave = m01.lambda
     template_flux = m01.(i)
  ENDIF
  
  IF (STRMID(comp.opt.template_id, 0, 5) EQ 'SWIRE') THEN BEGIN
     keys = swire.keys()
     keys = keys.toArray()
     i = WHERE(keys EQ STRUPCASE(STRMID(comp.opt.template_id, 6)))
     IF (i[0] EQ -1) THEN $
        MESSAGE, 'Uknown galaxy template: ' + comp.opt.template_id
     tmp = swire[keys[i[0]]]
     
     template_wave = tmp.x
     template_flux = tmp.y
  ENDIF

  ;;Interpolate on current wavelength grid
  cdata = INTERPOL(template_flux, template_wave, x) > 0

  l0 = [3650, 4400, 5100, 5500]
  ww = [680, 980, 100, 890]
  FOR i=0, gn(l0)-1 DO BEGIN
     qsfit_log, '  galaxy template flux density at ' + gn2s(l0[i]) + 'AA = ' + $
                gn2s(INTERPOL(SMOOTH(template_flux, 3), template_wave, l0[i]))
  ENDFOR
  FOR i=0, gn(l0)-1 DO BEGIN
     j = WHERE(template_wave GE l0[i]-ww[i]/2.  AND  template_wave LE l0[i]+ww[i]/2.)
     tmp = INT_TABULATED(template_wave[j], template_flux[j])
     qsfit_log, '  galaxy template flux integrated in the range ' + $
                gn2s(l0[i]) + 'AA +/- ' + gn2s(ww[i]/2.)+'AA = ' + gn2s(tmp)
  ENDFOR

  RETURN, PTR_NEW(cdata)
END


FUNCTION qsfit_comp_galaxytemplate, x, norm, cdata=cdata, opt=opt
  COMPILE_OPT IDL2
  ON_ERROR, !glib.on_error
  RETURN, norm * (*cdata)
END
