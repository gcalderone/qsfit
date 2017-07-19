; *******************************************************************
; Copyright (C) 2016-2017 Giorgio Calderone
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
;  ggp_plot
;
;PURPOSE:
;  Insert plotting options (i.e. the arguments to the "plot" and
;  "splot" gnuplot commands) for a data set.
;
;PARAMETERS:
;  OPTIONS: (input, a scalar or array of either string or a structure
;            whose template is given by ggp_plot_struct).
;    The plotting option for a given data set name.  Data set name (as
;    returned by ggp_data, /getname=) may be given at the beginning of
;    the string or as the .name field of the structure.
;    
;EXAMPLES:
;  d = [1,2]
;  ggp_data, /clear, d, d, getname=name
;  ggp_plot, name + 'w lines title "My data set" lw 3'
;  ggp
;
;  or we may use the structure template returned by ggp_plot_struct:
;
;  d = [1,2]
;  ggp_data, /clear, d, d, getname=name
;  opt = ggp_plot_struct()
;  opt.name = name
;  opt.w = 'lines'
;  opt.t = 'My data set'
;  opt.lw = 3
;  ggp_plot, opt
;  ggp
;
PRO ggp_plot, options, MULTI=multi
  COMPILE_OPT IDL2
  ON_ERROR, !glib.on_error
  COMMON COM_GGP

  IF (~KEYWORD_SET(multi)) THEN multi = 0

  IF (gtype(options[0]) EQ 'STRING') THEN BEGIN
     plot = options
  ENDIF $
  ELSE BEGIN
     plot = STRARR(gn(options))

     FOR i=0, gn(options)-1 DO BEGIN
        FOR j=0, N_TAGS(options[0])-1 DO BEGIN
           ff = STRLOWCASE((TAG_NAMES(options[0]))[j])
           vv = options[i].(j)

           IF (ff EQ 't') THEN BEGIN
              IF (vv EQ '')  THEN  BEGIN
                 ff = ''
                 vv = 'notitle'
              ENDIF $
              ELSE $
                 vv = '"' + vv + '"'
           ENDIF

           IF (vv NE '') THEN BEGIN
              IF (ff EQ 'name') THEN BEGIN
                 ff = ''
                 vv = STRLOWCASE(vv)
              ENDIF

              IF (ff EQ 'lty')  THEN ff = 'lt'


              plot[i] += ' ' + ff + ' ' + vv
           ENDIF
        ENDFOR
     ENDFOR
  ENDELSE

  ggp_plot.add, plot, /extract
  ggp_plot_m.add, REPLICATE(multi[0], gn(plot)), /extract
END
