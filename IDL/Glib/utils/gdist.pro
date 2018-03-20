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
;  gdist
;
;PURPOSE:
;  Identifies distinct values in an array
;
;PARAMETERS:
;  INDATA (input, array of any type except structure)
;    Input data. Elements in the array must be sortable using SORT.
;
;  DISTINCT (output, array of the same type as INDATA)
;   Distinct values in input array.
;
;  COUNT (output, array of integer numbers with the same length as DISTINCT)
;    How many times each element in DISTINCT occurred in input data.
;
;  PRINT= (optional input, an integer number >= 1) 
;    Print a summary of distinct values and their frequency (using
;    gps).  If PRINT=1 print all distinct values.  If PRINT>1 print
;    all distinct values whose frequency is GE PRINT.
;
PRO gdist, _inData, distinct, count, PRINT=print
  COMPILE_OPT IDL2
  ON_ERROR, !glib.on_error

  distinct = []
  count = []
  IF (gn(_inData) EQ 0) THEN RETURN

  ;;Local copy
  inData = _inData

  count_NaN = 0
  IF (gtype(inData[0]) EQ 'STRING') THEN BEGIN
     inData = STRTRIM(inData, 2)
  ENDIF $
  ELSE BEGIN
     dummy = gsearch(~FINITE(inData), count=count_NaN)
     IF (count_NaN GT 0) THEN BEGIN
        i = WHERE(FINITE(inData))
        IF (i[0] EQ -1) THEN inData = []  $
        ELSE                 inData = inData[i]
     ENDIF
  ENDELSE

  IF (gn(inData) GT 0) THEN BEGIN
     ;;Sort array and find unique values
     inData = inData[SORT(inData)]
     u = UNIQ(inData)

     distinct = REPLICATE(_inData[0], N_ELEMENTS(u))
     count = LONARR(N_ELEMENTS(u))

     FOR i=0, N_ELEMENTS(u)-1 DO BEGIN
        distinct[i] = inData[u[i]]
        count[i] = (i EQ 0   ?   u[0]+1   :   u[i]-u[i-1])
     ENDFOR
  ENDIF

  
  IF (count_NaN GT 0) THEN BEGIN
     distinct = [distinct, gnan()]
     count = [count, count_NaN]
  ENDIF

  IF (N_PARAMS() EQ 1   AND   gn(print) EQ 0) THEN print=1
  IF (KEYWORD_SET(print)) THEN BEGIN
     str = REPLICATE({value: _inData[0], count: 0l}, N_ELEMENTS(count))
     str.value = distinct
     str.count = count

     i = WHERE(str.count GE print[0])
     IF (i[0] EQ -1) THEN RETURN
     str = str[i]

     gps, str, nan='NaN'
  ENDIF
END


