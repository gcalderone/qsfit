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
;  gtype
;
;PURPOSE:
;  Replacement for SIZE(/tname)
;
;PARAMETERS:
;  INPUT  (input, a scalar or array of any type)
;    The data whose type is being queried.
;
;  /INTEGER (keyword)
;    If given the function will return 1 if the input parameter is one
;    of the integer types, otherwise returns 0. 
;
;  /FLOAT (keyword)
;    If given the function will return 1 if the input parameter is
;    a floating point number, otherwise returns 0. 
;
;  /STRUCT=  (keyword)
;    If given the function will return 1 if the input parameter is
;    a structure, otherwise returns 0. 
;
;RETURN VALUE: (a scalar string)
;  If no keyword is given returns a string with the name of the data
;  type.  If one keyword is given it returns either 0 or 1.
;
FUNCTION gtype, input, INTEGER=integer, FLOAT=float, STRUCT=struct
  COMPILE_OPT IDL2
  ON_ERROR, !glib.on_error

  type = SIZE(input, /tname)

  IF (KEYWORD_SET(integer)) THEN $
     RETURN, (type EQ 'BYTE')     OR   $
             (type EQ 'INT' )     OR   $
             (type EQ 'UINT')     OR   $
             (type EQ 'LONG')     OR   $
             (type EQ 'ULONG')    OR   $
             (type EQ 'LONG64')   OR   $
             (type EQ 'ULONG64')


  IF (KEYWORD_SET(float)) THEN $
     RETURN, (type EQ 'FLOAT' )   OR   $
             (type EQ 'DOUBLE')

  IF (KEYWORD_SET(struct)) THEN $
     RETURN, (type EQ 'STRUCT' )

  RETURN, type
END
