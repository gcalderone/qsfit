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
;  gcfmt
;
;PURPOSE:
;  Generate a format code (to be used by FORMAT= keywords) using a
;  C-style PRINTF format
;
;PARAMETERS:
;  FMT (input, a scalar string)
;    C-style PRINTF format (must include the "%" character).
;
;RETURN VALUE:
;   Format to be used with FORMAT= keywords.
;
;EXAMPLE:
;  PRINT, format=gcfmt('%4.2f\n'), 0.1
;
FUNCTION gcfmt, fmt
  RETURN, '($, %"' + fmt + '")'
END
