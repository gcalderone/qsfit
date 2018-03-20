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
;NAME:
;  gfit_report
;
;PURPOSE:
;  Print a report of current GFIT status.  Printing is performed
;  through gprint.
;
;PARAMETERS:
;  /ALL (keyword)
;    If given print all model parameters, otherwise only the free and
;    untied parameters will be printed.
;
;SEE ALSO:
;  utils/gprint.pro
;
PRO gfit_report, ALL=all
  COMPILE_OPT IDL2
  ON_ERROR, !glib.on_error
  COMMON GFIT

  gprint, '====== GFIT REPORT (Process id:' + gn2s(gfit.opt.pid) + ') ======'
  gprint
  gprint, CFORMAT='%14s :  %s\n', 'Data type', gfit.opt.data_type
  gprint
  gprint, CFORMAT='%14s :  %d\n', 'Components', N_TAGS(gfit.comp)
  FOR i=0, N_TAGS(gfit.comp)-1 DO BEGIN
     gprint, CFORMAT='%14s :  %-30s   %s\n' $
             , (TAG_NAMES(gfit.comp))[i], gfit.comp.(i).funcName, (gfit.comp.(i).enabled ? '' : 'DISABLED')
  ENDFOR

  ndata = 0l
  gprint
  gprint, CFORMAT='%14s :  %s\n', 'Observation:', 'Expressions'
  FOR iobs=0, N_TAGS(gfit.obs)-1 DO BEGIN
     obs = gfit.obs.(iobs)
     gprint, CFORMAT='%14d :  %s\n', iobs, obs.expr
     FOR iaux=0, N_TAGS(obs.aux)-1 DO $
        gprint, CFORMAT='%14s :  %s\n', (TAG_NAMES(obs.aux))[iaux], obs.aux.(iaux).expr
     ndata += gn(obs.eval.x)
  ENDFOR

  gprint
  par = gfit_get_par()
  IF (gn(par) EQ 0) THEN RETURN

  IF (KEYWORD_SET(all)) THEN $
     gprint, 'List of ALL parameters:' $
  ELSE BEGIN
     IF (~gsearch(par.fixed EQ 0  AND  par.tied EQ '', i)) THEN RETURN
     par = par[i]
     gprint, 'List of FREE parameters:'
  ENDELSE
  tmp  = gstru_sub(par, drop=['limited', 'step', 'fixed'])
  tmp2 = REPLICATE(CREATE_STRUCT(tmp[0], 'fixed', '', 'step', ''), gn(tmp))
  STRUCT_ASSIGN, par, tmp2

  tmp2.fixed = ''
  IF (gsearch(par.fixed, i)) THEN $
     tmp2[i].fixed = 'fixed'

  tmp2.step = ''
  IF (gsearch(par.step NE 0, i)) THEN $
     tmp2[i].step = gn2s(par[i].step, nan=' ')
  gps, tmp2, row=KEYWORD_SET(all)

  gprint
  gprint, CFORMAT='%14s :  %-10.3g   red.: %-5.2g\n' $
         , 'Fit stat.', gfit.res.fit_stat, gfit.res.fit_stat / ndata
  gprint, CFORMAT='%14s :  %d\n', 'N data', ndata
  gprint
  gprint, CFORMAT='%14s :  %-10.3g   red.: %-5.2g   Prob(worse fit): %-10.3g\n' $
         , 'Test stat.', gfit.res.test_stat, gfit.res.test_stat / gfit.res.test_dof, gfit.res.test_prob
  gprint, CFORMAT='%14s :  %-10d\n'  , 'DOF'          , gfit.res.test_dof
  gprint
  gprint, CFORMAT='%14s :  %-10d\n'  , 'MPFIT status' , gfit.res.mpfit_status
  gprint, CFORMAT='%14s :  %-10.3g\n', 'Elapsed time' , gfit.res.elapsed_time

  ;gprint, CFORMAT='%14s : %-10.6g   Ndata: %7d   DOF: %d\n' $
  ;       , 'Fit stat.', gfit.res.fit_stat, ndata, gfit.res.test_dof
  ;gprint, CFORMAT='%14s : %-10.6g   Red. : %-7.3g   Prob.: %-10.4g\n' $
  ;       , 'Test stat.', gfit.res.test_stat, gfit.res.test_stat / gfit.res.test_dof, gfit.res.test_prob
  ;gprint
  ;gprint, CFORMAT='%14s : %-10d\n'  , 'MPFIT status' , gfit.res.mpfit_status
  ;gprint, CFORMAT='%14s :d  %-10.3g\n', 'Elapsed time' , gfit.res.elapsed_time
  gprint
END
