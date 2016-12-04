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
;  gisunix (glib/utils)
;
;PURPOSE:
;  Check whether current platform is UNIX-like
;
;PARAMETERS:
;  NONE
;
;RETURN VALUE: (either 0 or 1)
;  Return 1 if the platform is UNIX-like, 0 otherwise.
;
FUNCTION gisunix
  RETURN, (STRUPCASE(!VERSION.OS_FAMILY) EQ 'UNIX')
END
