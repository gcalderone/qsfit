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
  gprint, CFORMAT='%14s :  %s', 'Data sets'
  
  tmp = []
  FOR i=0, gfit.data.nn-1 DO $
     tmp = [tmp, STRUPCASE(gfit.data.(i).label) + ' (' + gn2s(gn(gfit.data.(i).y)) + ')']
  gprint, STRJOIN(tmp, ', ')

  gprint
  gprint, CFORMAT='%14s :  %d\n', 'Components', gfit.comp.nn

  FOR i=0, gfit.comp.nn-1 DO BEGIN
     gprint, CFORMAT='%14s :  %-30s   %s\n' $
            , gfit.comp.(i).name, gfit.comp.(i).funcName, (gfit.comp.(i).enabled ? '' : 'DISABLED')
  ENDFOR

  gprint
  gprint, CFORMAT='%14s :\n', 'Expressions'  
  ;;Secondary expressions
  FOR i=0, gfit.data.nn-1 DO BEGIN
     gprint, CFORMAT='%14s : MODEL = %s\n', (TAG_NAMES(gfit.expr))[i], gfit.expr.(i).model

     FOR j=1, N_TAGS(gfit.expr.(i))-1 DO $
        gprint, CFORMAT='%14s   %s = %s\n', '', (TAG_NAMES(gfit.expr.(i)))[j], gfit.expr.(i).(j)
  ENDFOR


  gprint
  gprint, 'List of model parameters:'

  par = gfit_get_par(compenabled)

  IF (~KEYWORD_SET(all)) THEN BEGIN
     par = par[WHERE(compenabled)]
     par = par[WHERE(~par.fixed)]
     par = par[WHERE(par.tied EQ '')]
  ENDIF

  tmp  = gstru_sub(par, drop=['limited', 'step', 'fixed'])
  tmp2 = REPLICATE(CREATE_STRUCT(tmp[0], 'fixed', ''), gn(tmp))
  STRUCT_ASSIGN, par, tmp2

  tmp2.fixed = ''
  IF (gsearch(par.fixed, i)) THEN $
     tmp2[i].fixed = 'fixed'
  gps, tmp2, row=KEYWORD_SET(all)

  ndata = 0l
  FOR i=0, gfit.data.nn-1 DO $
     ndata += gn(gfit.cmp.(i))
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
  gprint
END
