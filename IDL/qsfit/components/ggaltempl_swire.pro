; *******************************************************************
; Copyright (C) 2016,2017 Giorgio Calderone
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


FUNCTION ggaltempl_swire
  COMPILE_OPT IDL2
  ON_ERROR, !glib.on_error

  path = FILE_DIRNAME(ROUTINE_FILEPATH('ggaltempl_swire', /is_function)) + PATH_SEP()
  path += 'swire' + PATH_SEP()

  files = [ $
          "Ell13_template_norm.sed", $
          "Ell2_template_norm.sed", $
          "Ell5_template_norm.sed", $
          "S0_template_norm.sed", $
          "Sa_template_norm.sed", $
          "Sb_template_norm.sed", $
          "Sc_template_norm.sed", $
          "Sdm_template_norm.sed", $
          "Sd_template_norm.sed", $
          "Spi4_template_norm.sed", $
          "N6090_template_norm.sed", $
          "M82_template_norm.sed", $
          "Arp220_template_norm.sed", $
          "I20551_template_norm.sed", $
          "I22491_template_norm.sed", $
          "BQSO1_template_norm.sed", $
          "I19254_template_norm.sed", $
          "Mrk231_template_norm.sed", $
          "N6240_template_norm.sed", $
          "QSO1_template_norm.sed", $
          "QSO2_template_norm.sed", $
          "Sey18_template_norm.sed", $
          "Sey2_template_norm.sed", $
          "Torus_template_norm.sed", $
          "TQSO1_template_norm.sed" ]

  ;;All SEDs are given in erg cm^-2 s^-1 AA^-1, normalized at 5500AA
  data = HASH()
  template = {x: 0., y: 0.}
  FOR i=0, N_ELEMENTS(files)-1 DO BEGIN                                    & $
     name = STRUPCASE(STRMID(files[i], 0, STRPOS(files[i], '_')))          & $
     sed = greadtexttable(path + files[i], ' ', /dropnull, templ=template) & $
     sed.y /= INTERPOL(SMOOTH(sed.y, 3), sed.x, 5500)                      & $
     data[name] = sed                                                      & $
  ENDFOR

  RETURN, data
END



PRO gswire_galtempl_plot
  COMPILE_OPT IDL2
  ON_ERROR, !glib.on_error

  ;;Swire galaxy templates:
  ;;http://www.iasf-milano.inaf.it/~polletta/templates/swire_templates.html
  gal = ggaltempl_swire()
  keys = gal.keys()

  ggp_clear
  ggp_cmd, /xlog, /ylog, xr=[3.d13, 3.d15];, yr=[1.e-4, 1]
  ggp_cmd, 'set term wxt'
  ggp_cmd, 'set key horizontal outside'
  ggp_cmd, 'set mxtics 10'
  ggp_cmd, 'set mytics 10'
  ggp_cmd, 'set format y "%3.0e"'
  ggp_cmd, xtit='Freq. [Hz]', ytit='{/Symbol n} F_{/Symbol n} [arb. units]'; [erg s^{-1} cm^{-2}]'
  stop
  FOR i=0, N_ELEMENTS(keys)-1 DO BEGIN
     x = gal[keys[i]].x
     y = gal[keys[i]].y
     ggp_data, 3.d18 / x, x*y, plot='w l title "' + keys[i] + '"'
  ENDFOR
  ggp
END
