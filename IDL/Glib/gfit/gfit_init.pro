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



PRO gfit_delete_cdata
  COMPILE_OPT IDL2
  ON_ERROR, !glib.on_error
  COMMON GFIT, gfit, gfit_cdata, gfit_wdev, cachePar
  
  FOR i=0, N_TAGS(gfit_cdata)-1 DO BEGIN
     IF (PTR_VALID(gfit_cdata.(i))) THEN $
        PTR_FREE, gfit_cdata.(i)
  ENDFOR
  gfit_cdata = {}
END


;=====================================================================
;NAME:
;  gfit_init
;
;PURPOSE:
;  Initialize GFIT status.
;
;PARAMETERS:
;  NONE
;
PRO gfit_init
  COMPILE_OPT IDL2
  ON_ERROR, !glib.on_error
  COMMON GFIT, gfit, gfit_cdata, gfit_wdev, cachePar
  
  gfit_delete_cdata

  gfit = { $
         opt: { pid:        0l    , $ 
                tol:        1.e-6 , $
                data_type: 'gauss', $ ;;Statistic appropriate for data, either 'gauss' or 'poisson'
                gof_test:   1b    , $ ;;perform Goodness-of-fit test (i.e. call gfit_teststat_*)
                log_iter:   1b    , $ ;;Log fit progress in gfit_iterproc
                evalAux:    0b      $
              }   , $
         comp: 0  , $
         obs:0    , $
         res:  { fit_stat:  gnan(), $                                   ;;Fit statistic
                 test_stat: gnan(), test_dof: 0l, test_prob: gnan() , $ ;;Test statistic
                 mpfit_status: 0l , $                                   ;;MPFIT status
                 elapsed_time: 0. , $                                   ;;Fitting elapsed time
                 covar: PTR_NEW()   $                                   ;;Covariance matrix
               } $
         }
END
