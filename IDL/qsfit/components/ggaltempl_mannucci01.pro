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


;=====================================================================
;NAME:
;  ggaltempl_mannucci01
;
;PURPOSE:
;  Read galaxy templates from Mannucci et al. 2001
;
;NOTES:
;  To plot the galaxy templates:
;    gal = ggaltempl_mannucci01()
;    ggp_cmd, /clear, xtitle='log Freq. [Hz]', ytitle='log nu L_{nu} [arb. units]'
;    FOR i=2, N_TAGS(gal)-1 DO $
;       ggp_data, ALOG10(gal.nu), ALOG10(gal.lambda * gal.(i)) $
;                 , plot='w l title "' + (TAG_NAMES(gal))[i] + '"'
;    ggp
;
;REFERENCES:
;  Mannucci et al. 2001, MNRAS, 326, 745M
;  ADS: http://adsabs.harvard.edu/abs/2001MNRAS.326..745M
;
FUNCTION ggaltempl_mannucci01
  COMPILE_OPT IDL2
  ON_ERROR, !glib.on_error

  path = FILE_DIRNAME(ROUTINE_FILEPATH('ggaltempl_mannucci01', /is_function)) + PATH_SEP()
  template = { lambda: 0., e: 0., s0: 0., sa: 0., sb: 0., sc: 0. }
  data = greadtexttable(path + 'ggaltempl_mannucci01.txt', ' ', /drop, template=template)
  data.lambda *= 1.e4    ;;Wavelength in angstrom
  data = gstru_insert(data, 'nu', 0., 1)
  data.nu = 3.e18 / data.lambda

   ;;All SEDs are given in erg cm^-2 s^-1 AA^-1, normalized at 5500AA
  FOR i=2, N_TAGS(data)-1 DO $
     data.(i) /= INTERPOL(SMOOTH(data.(i), 3), data.lambda, 5500)

  RETURN, data
END
