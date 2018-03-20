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
;  ggp_set_state
;
;PURPOSE:
;  Set current state according to the structure given as input.  The
;  state to use as argument can be retrieved using ggp_get_state().
;
;PARAMETERS:
;  STR: (input, scalar structure)
;    A structure with the state to be loaded, as returned by
;    ggp_get_state.
;
PRO ggp_set_state, str
  COMPILE_OPT IDL2
  ON_ERROR, !glib.on_error
  COMMON COM_GGP

  ggp_clear
  ;;FOR i=0, gn(str._cmd )-1 DO IF (str._cmd [i] NE '') THEN ggp_cmd.add , str._cmd [i]
  ;;FOR i=0, gn(str._plot)-1 DO IF (str._plot[i] NE '') THEN ggp_plot.add, str._plot[i]
  FOR i=0, gn(str._cmd   )-1 DO ggp_cmd.add   , str._cmd[i]
  FOR i=0, gn(str._cmd_m )-1 DO ggp_cmd_m.add , str._cmd_m[i]
  FOR i=0, gn(str._plot  )-1 DO ggp_plot.add  , str._plot[i]
  FOR i=0, gn(str._plot_m)-1 DO ggp_plot_m.add, str._plot_m[i]

  IF (N_TAGS(str) GT 2) THEN $
     ggp_data = gstru_sub(str, drop=['_CMD', '_CMD_M', '_PLOT', '_PLOT_M'])
END
