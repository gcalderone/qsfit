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
;  gfit_plot_resid
;
;PURPOSE:
;  Prepare the plots of residuals between the data and the GFIT model
;  using the GGP facility.  A further call to "ggp" is required to
;  actually do the plot.
;
;PARAMETERS:
;  IDATA (optional input, a scalar integer)
;    The index of the data set to plot.  If not given the first
;    dataset (i.e. IDATA=0) is assumed.
;
PRO gfit_plot_resid, OBS=iobs
  COMPILE_OPT IDL2
  ON_ERROR, !glib.on_error
  COMMON GFIT

  ;;Evaluate model
  gfit_run, /eval

  IF (gn(iobs) EQ 0) THEN iobs = 0
  obs = gfit.obs.(iobs)

  ggp_clear
  ggp_cmd, 'set bars 0'
  ggp_cmd, 'set grid'
  ggp_cmd, 'set  title "' + obs.plot.title + '"'
  ggp_cmd, 'set xlabel "' + obs.plot.xtit  + '"'
  ggp_cmd, 'set ylabel "Residuals [{/Symbol s}]"'
  ggp_cmd, 'set xrange [' + gn2s(MIN(obs.eval.x)) + ':' + gn2s(MAX(obs.eval.x)) + ']'
  IF (obs.plot.xlog) THEN ggp_cmd, 'set logscale x'

  x = obs.eval.x
  y = (obs.eval.y - obs.eval.m) / obs.eval.e
  gfit_rebin, obs.plot.rebin, x, y
  ggp_data, x, y, plot='w points notitle pt 1 ps 0.7 lc rgb "black"'
  ;;ggp_data, eval.x, y, REPLICATE(1., gn(y)), plot='with yerrorbars notitle lt rgb "gray"'

  ;;Horizontal "zero" line
  ggp_data, name='zero', gminmax(obs.eval.x), [0,0]
  ggp_plot, '$zero w line notitle dt 2 lw 2 lt rgb "orange"'

  ;;Cumulative reduced fit statistic
  fs = TOTAL(y^2., /cumulative) / gfit.res.test_dof
  ggp_cmd, 'set y2label "Cumulative {/Symbol c}^2_{red}"'
  ggp_cmd, 'set ytics nomirror'
  ggp_cmd, 'set y2tics'
  ggp_cmd, 'set format y2 "%.1f'
  ggp_cmd, 'set y2range [' + gn2s(MIN(fs)) + ':' + gn2s(MAX(fs)) + ']'
  ggp_cmd, 'set key bottom right'

  ggp_data, name='cfs', obs.eval.x, fs
  ggp_plot, '$cfs w l title "Reduced cumul. {/Symbol c}^2" ls 1 lw 2 lt rgb "red" axes x1y2'
END




