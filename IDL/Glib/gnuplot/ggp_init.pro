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
;  ggp_init
;
;PURPOSE:
;  Initialize the data in the COM_GGP common block.  It is
;  called by all ggp_* routines and should not be called
;  directly by the user.
;
;PARAMETERS:
;  NONE
;
PRO ggp_init
  COMPILE_OPT IDL2
  ON_ERROR, !glib.on_error
  COMMON COM_GGP, ggp_flagInit, ggp_pid, ggp_data, ggp_cmd, ggp_plot

  IF (gn(ggp_flagInit) EQ 0) THEN BEGIN
     ggp_pid  = LIST()
     ggp_data = []
     ggp_cmd  = LIST()
     ggp_plot = LIST()
     ggp_flagInit = 1
  ENDIF
END


