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
;  gps
;
;PURPOSE:
;  Pretty print a [array of] structure.  Printing is perfromed through
;  gprint.
;
;PARAMETERS:
;  STR (input, a scalar or array of structure)
;    Structure whose content is to be printed.
;
;  LUN= (keyword, a logical unit number)
;    Forwarded to gprint.
;
;  HEAD= (optional input, either 0 or 1)
;    Set to 1 to print header, 0 to skip header. Default is 1.
;
;  ROWID= (optional input, either 0 or 1)
;    Set to 1 to print row progressive number, 0 to skip it. Default is 1.
;
;  /TYPE (keyword)
;    Print data type after header.
;
;  FSEP= (optional, a scalar string)
;    Field separator.  Default is "|".
;
;  BOR= (optional, a scalar string)
;    Marker of beginning of record.  Default is an empty string.
;
;  EOR= (optional, a scalar string)
;    Marker of end of record.  Default is an empty string.
;
;  /FAST (keyword)
;    Passed to gn2s (avoid deleting non-significant zeroes when printing numbers).
;
;  NAN= (optional input, a scalar string)
;    Passed to gn2s (string representation of NaN numbers).
;
;  SKIP= (optional input, an array of integer numbers)
;    Insert a blank line at given indices
;
;  OUT= (output, an array of string)
;    Return an array of strings instead of printing.
;
PRO gps, str                             $
   , LUN=lun, FAST=fast, NAN=nan         $
   , HEAD=head, ROWID=rowid, TYPE=ptype  $
   , FSEP=_fsep, BOR=bor, EOR=eor        $
   , SKIP=iskip , OUT=out
  COMPILE_OPT IDL2
  ON_ERROR, !glib.on_error

  IF (gtype(str[0]) NE 'STRUCT') THEN $
     MESSAGE, 'GPS require a structure as input parameter'

  IF (gn(head)  EQ 0) THEN head  = 1
  IF (gn(ptype) EQ 0) THEN ptype = 0
  IF (gn(rowid) EQ 0) THEN rowid = 1
  head  = KEYWORD_SET(head)
  rowid = KEYWORD_SET(rowid)
  IF (ptype) THEN head=1

  IF (~KEYWORD_SET(nan)) THEN nan = ' '

  IF (gn(_fsep) EQ 0)     THEN fsep = '|' $
  ELSE                         fsep = STRING(_fsep[0])

  IF (~KEYWORD_SET(bor))  THEN bor  = ''
  IF (~KEYWORD_SET(eor))  THEN eor  = ''


  ;;--------------------------------------------------
  nrec = gn(str)
  nfld = N_TAGS(str)
  tagn = TAG_NAMES(str)

  typename = STRARR(nfld)
  FOR i=0, nfld-1 DO BEGIN
     typename[i] = gtype(str[0].(i))
     IF (typename[i] EQ 'INT') THEN $
        typename[i] = 'FIX'

     IF (gn(str[0].(i)) GT 1) THEN $
        typename[i] += ' ' + gn2s(gn(str[0].(i)))
  ENDFOR


  ;;Buffer
  buf = STRARR(nrec+2, nfld+1)

  ;;Add rowID
  nn = CEIL(ALOG10(nrec))
  buf[0:nrec-1, 0] = STRING(FORMAT=gcfmt('%' + gn2s(nn) + 'd'), LINDGEN(nrec))
  buf[  nrec  , 0] = STRING(FORMAT=gcfmt('%' + gn2s(nn) + 's'), '#')
  buf[  nrec+1, 0] = STRING(FORMAT=gcfmt('%' + gn2s(nn) + 's'), '#')



  FOR j=0, nfld-1 DO BEGIN
     nn = gn(str[0].(j))
     ;;IF   (nn GT 5)                            THEN type = 0  $
     IF      (gtype((str[0].(j))) EQ 'STRING') THEN type = 'STRING' $
     ELSE IF (gtype((str[0].(j)), /integer  )) THEN type = 'INT'    $
     ELSE IF (gtype((str[0].(j)), /float    )) THEN type = 'FLOAT'  $
     ELSE IF (gtype((str[0].(j))) EQ 'STRUCT') THEN type = 'STRUCT' $
     ELSE $
        MESSAGE, 'Unhandled data type: ' + gtype(str[0].(j))

     IF (nn GT 5) THEN BEGIN
        SWITCH (type) OF
           'STRING': BEGIN
              tmp = '...'
              BREAK
           END
           'STRUCT': BEGIN
              tmp = '{struct}'
              BREAK
           END
           'INT':
           'FLOAT': BEGIN
              ;;tmp = gminmax(str.(j), dim=1)
              ;;dim = gdim(tmp)
              ;;tmp = gn2s(tmp)
              ;;tmp = REFORM(tmp, dim)
              ;;len = MAX(STRLEN(tmp), dim=2)
              ;;tmp[0,*] = STRING(FORMAT=gcfmt('%' + gn2s(len[0]) + 's'), tmp[0,*])
              ;;tmp[1,*] = STRING(FORMAT=gcfmt('%' + gn2s(len[1]) + 's'), tmp[1,*])
              ;;tmp = REFORM(tmp[0,*] + ' : ' + tmp[1,*])

              min = gn2s(MIN(str.(j)))
              max = gn2s(MAX(str.(j)))
              min = STRING(FORMAT=gcfmt('%' + gn2s(MAX(STRLEN(min))) + 's'), min)
              max = STRING(FORMAT=gcfmt('%' + gn2s(MAX(STRLEN(max))) + 's'), max)
              tmp = REFORM(min + ':' + max)
           END
        ENDSWITCH
     ENDIF $
     ELSE BEGIN
        CASE (type) OF
           'STRING':  tmp =      str.(j)
           'INT':     tmp = gn2s(str.(j), fast=fast)
           'FLOAT':   tmp = gn2s(str.(j), fast=fast, nan=nan)
           'STRUCT':  tmp = '{struct}'
        ENDCASE
     ENDELSE

     ;;Character length of field
     len = STRLEN(tmp)
     len = MAX(len)

     ;;Transform fields with arrays into scalar
     IF ((gn(gsize(tmp)) GT 1)                OR $
         (gn(tmp) GT 1   AND   nrec EQ 1))    $ ;;... if input structure is a scalar with vector field
     THEN BEGIN
        FOR i=0, nrec-1 DO $
           tmp[0,i] = STRJOIN(STRING(FORMAT=gcfmt('%' + gn2s(len) + 's'), tmp[*,i]), ', ')
        tmp = REFORM(tmp[0,*])
        len = STRLEN(tmp)
     ENDIF

     len = MAX(len)
     len += 1

     IF (head) THEN $ ;;consider header length
        len = MAX([len, STRLEN(tagn[j])])

     IF (ptype) THEN $ ;;consider type name length
        len = MAX([len, STRLEN(typename[j])])

     fmt = gcfmt('%'  + gn2s(len) + 's')
     IF (type EQ 'STRING') THEN $
        fmt = gcfmt('%-' + gn2s(len) + 's')

     buf[0:nrec-1, j+1] = STRING(FORMAT=fmt, tmp)
     buf[nrec    , j+1] = STRING(FORMAT=fmt, tagn[j])
     buf[nrec+1  , j+1] = STRING(FORMAT=fmt, typename[j])
  ENDFOR

  ;;Map of blank records
  skip = REPLICATE(0b, nrec)
  IF (gn(iskip) NE 0) THEN skip[iskip] = 1

  ;;Output variable
  out = STRARR(nrec+head+ptype+TOTAL(skip))
  i0 = (rowid + 1) MOD 2

  ;;Header
  IF (head) THEN BEGIN
     out[0] = bor + STRJOIN(REFORM(buf[nrec,i0:*]), fsep) + eor

     IF (~ARG_PRESENT(out)) THEN $
        gprint, LUN=lun, STRTRIM(out[0])
  ENDIF

  ;;
  IF (ptype) THEN BEGIN
     out[head] = bor + STRJOIN(REFORM(buf[nrec+1,i0:*]), fsep) + eor

     IF (~ARG_PRESENT(out)) THEN $
        gprint, LUN=lun, STRTRIM(out[head])
  ENDIF

  ;;Contents
  nskip = 0l
  FOR i=0, nrec-1 DO BEGIN
     j = head+ptype+i+nskip
     out[j] = bor $
              + STRJOIN(REFORM(buf[i,i0:*]), fsep) $
              + eor

     IF (~ARG_PRESENT(out)) THEN $
        gprint, LUN=lun, STRTRIM(out[j])

     IF (skip[i]) THEN BEGIN
        IF (~ARG_PRESENT(out)) THEN $
           gprint, LUN=lun
        nskip += 1
     ENDIF
  ENDFOR
END
