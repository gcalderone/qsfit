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
;  ggp_clear
;
;PURPOSE:
;  (Re-)Initialize the state of the GGP package.  It should be called
;  by the user before each new plot.
;
;PARAMETERS:
;  NONE
;
PRO ggp_clear
  COMPILE_OPT IDL2
  ON_ERROR, !glib.on_error
  COMMON COM_GGP, $
     ggp_data,             $
     ggp_cmd,  ggp_cmd_m,  $
     ggp_plot, ggp_plot_m

  ggp_data   = []
  ggp_cmd    = LIST()
  ggp_cmd_m  = LIST()
  ggp_plot   = LIST()
  ggp_plot_m = LIST()
END

