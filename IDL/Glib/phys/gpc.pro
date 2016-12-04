; *******************************************************************
; Copyright (C) 2016 Giorgio Calderone
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
;  gpc
;
;PURPOSE:
;  Return a structure with several physical constants
;
FUNCTION gpc
  COMPILE_OPT IDL2
  ON_ERROR, !glib.on_error
  COMMON COM_GPC, c

  IF (gn(c) EQ 1) THEN RETURN, c

  c = { $
  ;
  ;Ref: http://www.astro.wisc.edu/~dolan/constants.html
  ;

  ;Dimensionless or common to both CGS and MKS
  jd_mjd          : 2400000.5d          , $   ;Offset to change from Julian date to MJD
  NA              : 6.0221367d   * 1e23 , $   ;Avagadro's number
  alpha           : 7.29735308d  * 1e-3 , $   ;Fine structure constant
  day             : 86400d              , $   ;days to sec fator
  month           : 2592000d            , $   ;months to sec fator
  year            : 31536000.d          , $   ;year to sec fator

  ;CGS               
  c               : 2.99792458d * 1e10  , $   ;Vacuum speed of light (cmd s^-1)
  h               : 6.6260755d  * 1e-27 , $   ;Planck's constant (erg s)
  hbar            : 1.05457266d * 1e-27 , $   ;reduced Planck's constant (erg s)
  G               : 6.67259d    * 1e-8  , $   ;Gravitational constant (cm3 g^-1 s^-2)
  e               : 4.8032068d  * 1e-10 , $   ;Electron charge (esu)
  me              : 9.1093897d  * 1e-28 , $   ;Mass of electron (g)
  mp              : 1.6726231d  * 1e-24 , $   ;Mass of proton (g)
  mn              : 1.6749286d  * 1e-24 , $   ;Mass of neutron (g)
  mH              : 1.6733d     * 1e-24 , $   ;Mass of hydrogen (g)
  amu             : 1.6605402d  * 1e-24 , $   ;Atomic mass unit (g)
  k               : 1.380658d   * 1e-16 , $   ;Boltzmann constant (erg k^-1)
  ev              : 1.6021772d  * 1e-12 , $   ;Electron volt to erg factor (erg eV^-1)
  a               : 7.5646d     * 1e-15 , $   ;Radiation density constan (erg cm^-3 K^-4)
  sigma           : 5.67051d    * 1e-5  , $   ;Stefan-Boltzmann constant(erg cm^-2 K^-4 s^-1)
  R_inf           : 2.1798741d  * 1e-11 , $   ;Rydberg constant (erg)
  au              : 1.496d      * 1e13  , $   ;Astronomical unit to cm factor (cm AU^-1)
  pc              : 3.086d      * 1e18  , $   ;Parsec to cm factor (cm pc^-1)
  ly              : 9.463d      * 1e17  , $   ;Light year to cm factor (cm ly^-1)
  ld              : 2.5902      * 1e15  , $   ;Light day to cm factor (cm ly^-1)
  sunmass         : 1.99d       * 1e33  , $   ;Solar mass (g)
  sunrad          : 6.96d       * 1e10  , $   ;Solar radius (cm)
  sunlum          : 3.9d        * 1e33  , $   ;Solar luminosity (erg s^-1)
  suntemp         : 5.780d      * 1e3   , $   ;Solar temperature (K)
  earthmass       : 5.974d      * 1e27  , $   ;Earth mass (g)
  earthrad        : 6372.8      * 1e5   , $   ;Earth mean radius (cm)
  eg              : 981.52              , $   ;Acceleration at Earth surface (cm s^-2)
  h0              : 71.d                , $   ;Hubble constant (Km s^-1 Mpc^-1), Komatsu        et al. 2009
  omegalambda     : 0.73                , $  
  omegamatter     : 0.27                , $  
  thom            : 0.66524616d * 1e-24 , $   ;Thomson cross section (cm^2)
  jansky          : 1.509d      * 1e3   , $   ;Flux density (keV cm^-2 s^-1 keV^/1)
  wien            : 0.d                 , $   ;Hz / K
  R_cm            : 0.d                 , $   ;Rydberg constant (cm^-1)
  R2_cm           : 0.d                 , $   ;Rydberg constant (cm^-1, reduced mass)
  deg             : !DPI/180.           , $   ;rad deg^-1
  arcsec          : !DPI/180./3600      , $   ;rad arcsec^-1
  mas             : !DPI/180./3600./1000, $   ;rad milliarcsec^-1
  edd             : 1.26d38               $   ;Eddington luminosity (erg s^-1 M_sun^-1)
      }

  c.h0 *= 1.e5 / c.pc / 1.e6
  c.wien = 2.82 * c.k / c.h

  c.r_cm = 2 * !DPI^2 * c.me * c.e^4 / (c.h^3) / c.c
  c.r2_cm = c.r_cm / (1 + c.me/c.mp)

  RETURN, c
END
