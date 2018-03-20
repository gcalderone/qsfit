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
;  ggp_cmd
;
;PURPOSE:
;  Insert a new gnuplot command into current GGP buffer.  The most
;  frequently used commands can be passed through keywords.
;
;PARAMETERS:
;  COMMANDS  (input, scalar or array of string)
;    Gnuplot commands (except "plot" and "splot").
;
;  /CLEAR  (keyword)
;    Call ggp_clear before any operation.
;
;  XRANGE= (optional input, 2 element array of numbers)
;    Set X axis range using "set xrange".  One of the input numbers
;    may be NaN to use autoscaling on one side of the range.
;
;  YRANGE=, ZRANGE=  (see XRANGE=)
;  
;  TITLE= (optional input, scalar string)
;    Set the plot title using "set title";
;
;  XTITLE= (optional input, scalar string)
;    Set the X axis label using "set xlabel".
;
;  YTITLE=, ZTITLE=  (see XTITLE=)
;
;  /XLOG (keyword)
;    Set logarithmic scale on X axis using "set logscale x".
;
;  /YLOG, /ZLOG  (see /XLOG)
;
;EXAMPLES:
;  d = [1,2]
;  ggp_cmd, /clear, title='My first plot', xr=[0,3], 'set key left'
;  ggp_data, d, d, plot='w lines'
;  ggp
;
PRO ggp_cmd        $
   , commands      $
   , CLEAR=clear, TITLE=title $
   , XRANGE=xrange, YRANGE=yrange, ZRANGE=zrange $
   , XTITLE=xtitle, YTITLE=ytitle, ZTITLE=ztitle $
   , XLOG=xlog, YLOG=ylog, ZLOG=zlog             $
   , MULTI=multi
  COMPILE_OPT IDL2
  ON_ERROR, !glib.on_error
  COMMON COM_GGP

  IF (KEYWORD_SET(clear)) THEN ggp_clear
  IF (~KEYWORD_SET(multi)) THEN multi = 0

  IF (gn(commands) GT 0) THEN BEGIN
     ggp_cmd.add, commands, /extract
     ggp_cmd_m.add, REPLICATE(multi[0], gn(commands)), /extract
  ENDIF

  add = LIST()
  IF (gn(xrange) EQ 2)   THEN add.add, 'set xrange [ ' + STRJOIN(gn2s(xrange, nan='*'), ' : ') + ' ]'
  IF (gn(yrange) EQ 2)   THEN add.add, 'set yrange [ ' + STRJOIN(gn2s(yrange, nan='*'), ' : ') + ' ]'
  IF (gn(zrange) EQ 2)   THEN add.add, 'set zrange [ ' + STRJOIN(gn2s(zrange, nan='*'), ' : ') + ' ]'

  IF (gn( title) EQ 1)   THEN add.add, 'set title  ''' + STRING( title) + ''''
  IF (gn(xtitle) EQ 1)   THEN add.add, 'set xlabel ''' + STRING(xtitle) + ''''
  IF (gn(ytitle) EQ 1)   THEN add.add, 'set ylabel ''' + STRING(ytitle) + ''''
  IF (gn(ztitle) EQ 1)   THEN add.add, 'set zlabel ''' + STRING(ztitle) + ''''

  IF (gn(xlog) EQ 1)     THEN add.add, (KEYWORD_SET(xlog)  ?  ''  :  'un') + 'set logscale x'
  IF (gn(ylog) EQ 1)     THEN add.add, (KEYWORD_SET(ylog)  ?  ''  :  'un') + 'set logscale y'
  IF (gn(zlog) EQ 1)     THEN add.add, (KEYWORD_SET(zlog)  ?  ''  :  'un') + 'set logscale z'

  IF (gn(add) GT 0) THEN BEGIN
     add = add.toArray()
     ggp_cmd.add, add, /extract
     ggp_cmd_m.add, REPLICATE(multi[0], gn(add)), /extract
  ENDIF
END
