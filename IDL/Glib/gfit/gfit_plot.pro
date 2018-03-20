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
;  gfit_plot
;
;PURPOSE:
;  Prepare the plots of GFIT data, model and secondary expressions
;  using the GGP facility.  A further call to "ggp" is required to
;  actually do the plot.
;
;PARAMETERS:
;  IDATA (optional input, a scalar integer)
;    The index of the data set to plot.  If not given the first
;    dataset (i.e. IDATA=0) is assumed.
;
PRO gfit_plot, OBS=iobs
  COMPILE_OPT IDL2
  ON_ERROR, !glib.on_error
  COMMON GFIT

  ;;Evaluate model
  gfit_run, /eval

  IF (gn(iobs) EQ 0) THEN iobs = 0
  obs = gfit.obs.(iobs)

  ;;MAIN PLOT =====
  ggp_clear
  ggp_cmd, 'set bars 0'
  ggp_cmd, 'set clip two'
  ggp_cmd, 'set grid'
  ggp_cmd, 'set  title "' + obs.plot.title + '"'
  ggp_cmd, 'set xlabel "' + obs.plot.xtit  + '"'
  ggp_cmd, 'set ylabel "' + obs.plot.ytit  + '"'
  ggp_cmd, 'set xrange [' + gn2s(MIN(obs.eval.x)) + ':' + gn2s(MAX(obs.eval.x)) + ']'
     
  IF (obs.plot.xlog) THEN ggp_cmd, 'set logscale x'
  IF (obs.plot.ylog) THEN ggp_cmd, 'set logscale y'

  FOR i=0, N_TAGS(obs.data)-1 DO BEGIN
     d = obs.data.(i)
     IF (~d.plot.enable) THEN CONTINUE
     IF (~gsearch(d.group GT 0, j)) THEN CONTINUE
     
     x = d.x[j]
     y = d.y[j]
     e = d.e[j]
     ;;gfit_rebin, obs.plot.rebin, x, y, e
     name = 'd' + gn2s(iobs) + '_' + gn2s(i)
     ggp_data, name=name, x, y, e
     ggp_plot, '$' + name + ' title "' + d.plot.label + '" ' + d.plot.gp     
  ENDFOR

  ;;Plot model
  ggp_data, name='model', obs.eval.x, obs.eval.m
  IF (obs.plot.gp EQ "") THEN obs.plot.gp = "with line"
  ggp_plot, '$model title "' + obs.plot.label + '" ' + obs.plot.gp

  ;;Plot auxiliary expressions
  FOR i=0, N_TAGS(obs.aux)-1 DO BEGIN
     IF (~obs.aux.(i).plot.enable) THEN CONTINUE
     name = 'a' + gn2s(iobs) + '_' + gn2s(i)
     tmp = obs.eval.aux.(i)
     IF (gn(tmp) EQ 1) THEN tmp = REPLICATE(tmp, gn(obs.eval.x))
     ggp_data, name=name, obs.eval.x, tmp
     ggp_plot, '$' + name + ' title "' + obs.aux.(i).plot.label + '" ' + obs.aux.(i).plot.gp
  ENDFOR
END




