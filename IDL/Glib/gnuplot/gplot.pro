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
;  gplot
;
;PURPOSE:
;  Easily use GGP with a single statement whose syntax is similar
;  (although not indentical) to the PLOT built-in command.
;
;DESCRIPTION:
;  This procedure allows to use GGP with a single statement whose
;  syntax is similar (although not indentical) to the PLOT built-in
;  command.  All required GGP routines (ggp_clear, ggp_data, ggp_cmd,
;  ggp_plot, ggp_hist and ggp) are called as needed.
;
;PARAMETERS:
;  X  (input, array of numbers)
;    The X coordinate values (if also Y is given) or the Y coordinate
;    values (if only X is given).
;
;  Y (optional input, array of numbers)
;    The Y coordinate values.
;
;  E  (optional input, array of numbers)
;    The errors associated to the Y values.
;
;  PLOT= (optional input, a scalar string)
;    The plotting options.  This string is passed to the PLOT= keyword
;    of ggp_data.
;
;  /OVERPLOT (keyword)
;    Avoid calling ggp_clear.  If not given ggp_clear wil be called
;    before any other operation.
;
;  XRANGE=, YRANGE=, XLOG=, YLOG=, XTITLE=, YTITLE=, TITLE=, COMMANDS=
;    All these keywords are passed to ggp_cmd (see ggp_cmd
;    documentation)
;
;  TERM=term, OUTPUT=output, NORUN=norun
;    These keywords are passed to ggp (see ggp documentation)
;
;  /HIST= (keyword)
;  Pass the X (and Y if given) parameter(s) to ggp_hist to compute a
;  1D or 2D histogram.
;
;  _EXTRA=extra
;    These keywords are passed to ggp_hist (see ggp_hist
;    documentation)
;
;EXAMPLES:
;  d = [1,2]
;  gplot, commands='set key left' $
;         , xtit='X label', ytit='Y label', title='My title' $
;         , xr=[0.8, 2.2] $
;         , d, d, plot='w l title "My data set"'
;
PRO gplot $
   , x, y, e, PLOT=plot, OVERPLOT=oplot         $
   , XRANGE=xrange, YRANGE=yrange               $
   , XLOG=xlog, YLOG=ylog                       $
   , XTITLE=xtitle, YTITLE=ytitle, TITLE=title  $
   , COMMANDS=commands                          $
   , TERM=term, OUTPUT=output, NORUN=norun      $
   , HIST=hist, _EXTRA=extra
  COMPILE_OPT IDL2
  ON_ERROR, !glib.on_error

  IF (~KEYWORD_SET(oplot)) THEN ggp_clear

  ggp_cmd, xr=xrange, yr=yrange, xlog=xlog, ylog=ylog  $
           , xtitle=xtitle, ytitle=ytitle, title=title
  IF (N_ELEMENTS(commands) NE 0) THEN $
     ggp_cmd, commands

  hist = KEYWORD_SET(hist)

  CASE (N_PARAMS()) OF
     0: MESSAGE, 'No input data given'
     1: BEGIN
        IF (hist) THEN $
           ggp_plot, ggp_hist(x, _extra=extra) $
        ELSE BEGIN
           IF (~KEYWORD_SET(plot)) THEN plot='w p notitle'
           ggp_data, FINDGEN(gn(x)), x, plot=plot
        ENDELSE
     END
     2: BEGIN
        IF (hist) THEN $
           ggp_plot, ggp_hist([[x], [y]], _extra=extra) $
        ELSE BEGIN
           IF (~KEYWORD_SET(plot)) THEN plot='w p notitle'
           ggp_data, x, y, plot=plot
        ENDELSE
     END
     3: BEGIN
        IF (hist) THEN $
           MESSAGE, 'Three arguments are incompatible with HIST keyword'
        IF (~KEYWORD_SET(plot)) THEN plot='w yerrorbars notitle'
        ggp_data, x, y, e, plot=plot
     END
  ENDCASE
  
  ggp, term=term, output=output, norun=norun
END
