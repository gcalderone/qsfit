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


PRO gfit_replace_obs, iobs, item
  COMPILE_OPT IDL2
  ON_ERROR, !glib.on_error
  COMMON GFIT
  IF (iobs LT 0  OR  $
      iobs GE N_TAGS(gfit.obs)) THEN $
     MESSAGE, 'IOBS=' + gn2s(iobs) + ' is not a valid ID'
  
  tmp = {}
  tagNames = TAG_NAMES(gfit.obs)
  FOR i=0, N_TAGS(gfit.obs)-1 DO $
     tmp = CREATE_STRUCT(tmp, tagNames[i], (i EQ iobs  ?  item  :  gfit.obs.(i)))
  gfit = {opt: gfit.opt, comp: gfit.comp, obs: tmp, res: gfit.res}
END



;=====================================================================
;NAME:
;  gfit_add_obs
;
;PURPOSE:
;
;DESCRIPTION:
;
;PARAMETERS:
;
PRO gfit_add_obs
  COMPILE_OPT IDL2
  ON_ERROR, !glib.on_error
  COMMON GFIT

  id = N_TAGS(gfit.obs)
  obs = { expr: "", aux: 0, data: 0, eval: 0, $
          plot: {rebin: 1l            , $                            ;;Rebin data for plotting (disabled if <= 1)
                 xlog:  0b            , $                            ;;Enable (1) or disable (0) the logarithmic X axis.
                 ylog:  0b            , $                            ;;Enable (1) or disable (0) the logarithmic Y axis.
                 xr:    gnan() * [1,1], $                            ;;Range for the X axis.  Use NaN to use autoscale.
                 yr:    gnan() * [1,1], $                            ;;Range for the Y axis.  Use NaN to use autoscale.
                 title: 'notitle'     , $                            ;;Plot title.
                 xtit:  'X'           , $                            ;;Label for X axis.
                 ytit:  'Y'           , $                            ;;Label for Y axis.
                 label: 'Model'+gn2s(id), $                          ;;Label for the obs
                 gp: 'with line ls 1 dt 1 lw 2 lt rgb "orange"' $    ;;Gnuplot format for the obs
                } $
        }
  
  tmp = (id EQ 0  ?  {i0: obs}  :  CREATE_STRUCT(gfit.obs, 'i'+gn2s(id), obs))
  gfit = {opt: gfit.opt, comp: gfit.comp, obs: tmp, res: gfit.res}
END
