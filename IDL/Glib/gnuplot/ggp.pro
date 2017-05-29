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
;  ggp
;
;PURPOSE:
;  Dump all data and commands accumulated in GGP internal buffer in a
;  temporary text file and spawn a gnuplot process passing the file
;  name as argument.
;
;PARAMETERS:
;  /SPLOT  (keyword)
;    Use "splot" command instead of "plot".
;
;  /STDOUT (keyword)
;    Print data and commands on standard output rather than in a file,
;    and do not spawn the gnuplot process.  This keyword is typically
;    used for debugging purposes.
;
;  GP= (optional input, a scalar string)
;    Use this string as filename to write gnuplot data and commands,
;    and avoid spawning the gnuplot process.  This file will not be
;    deleted.  If this keyword is not given the file "./idl.ggp" will
;    be used, a gnuplot process will be spawned and then the file will
;    be deleted.
;
;  TERMINAL= (optional input, a scalar string)  
;    The name and option of a terminal.  This string will be prepended
;    with "set term" and added to the list of commands.  If not given
;    the "wxt" terminal will be used.
;
;  OUTPUT= (optional input, a scalar string)  
;    The file name of the terminal output file. This string will be
;    prepended with "set output" and added to the list of commands.
;
PRO ggp                           $
   , SPLOT=splot                  $
   , STDOUT=stdout, GP=file_gp    $
   , TERMINAL=term, OUTPUT=file_out
  COMPILE_OPT IDL2
  ON_ERROR, !glib.on_error
  COMMON COM_GGP

  runGnuplot = 0

  IF (~KEYWORD_SET(file_gp)) THEN BEGIN
     file_gp = 'idl.ggp'
     runGnuplot = 1
  ENDIF

  IF (KEYWORD_SET(stdout)) THEN BEGIN
     lun = -1       ;;print on standard output
     runGnuplot = 0 ;;do not run gnuplot
  ENDIF $
  ELSE BEGIN
     ;;Open gp file for writing
     OPENW, lun, file_gp, /get_lun
  ENDELSE

  ;;Reset gnuplot session
  PRINTF, lun, 'reset session'

  ;;Write data
  FOR i=0, N_TAGS(ggp_data)-1 DO BEGIN
     name = (TAG_NAMES(ggp_data))[i]
     PRINTF, lun, '$' + STRLOWCASE(name) + ' << EOD'
     gps, ggp_data.(i), skip=keybreak $
          , head=0, fsep='  ', rowid=0 $
          , NaN='NaN', fast=(gn(dataset) GT 500) $
          , lun=lun
     PRINTF, lun, 'EOD'
     PRINTF, lun
  ENDFOR

  ;;Setup the terminal
  IF (~KEYWORD_SET(term)) THEN BEGIN
     ;term = 'wxt persist linewidth 2'
     term = 'qt'
  ENDIF
  PRINTF, lun, 'set term ' + term

  ;;Set output file
  IF (KEYWORD_SET(file_out)) THEN BEGIN
     PRINTF, lun, 'set output "' + file_out + '"'
  ENDIF $
  ELSE BEGIN
     PRINTF, lun, '#set term pdf'
     PRINTF, lun, '#set output "idlgp.pdf"'
  ENDELSE

  ;;Set grid by default
  PRINTF, lun, 'set grid'

  ;;Write commands
  FOR i=0, gn(ggp_cmd)-1 DO BEGIN
     PRINTF, lun, ggp_cmd[i]
  ENDFOR

  ;;Write plot command
  IF (gn(ggp_plot) GT 0) THEN BEGIN
     IF (KEYWORD_SET(splot)) THEN PRINTF, lun, 'splot  \' $
     ELSE                         PRINTF, lun, 'plot   \'
     FOR i=0, gn(ggp_plot)-1 DO BEGIN
        tmp = '  ' + ggp_plot[i]
        IF (i LT gn(ggp_plot)-1) THEN tmp += ' , \'
        PRINTF, lun, tmp
     ENDFOR
  ENDIF

  IF (KEYWORD_SET(file_out)) THEN BEGIN
     PRINTF, lun, 'set output' ;;close output file
  ENDIF $
  ELSE BEGIN
     PRINTF, lun, '#set output'
  ENDELSE

  IF (~KEYWORD_SET(stdout)) THEN $
     FREE_LUN, lun

  ;;Spawn gnuplot process
  IF (runGnuplot) THEN BEGIN
     gpcmd = 'gnuplot  -persist "' + file_gp + '"' ;;default gnuplot invocation
     IF (gisunix()) THEN SPAWN, gpcmd, /NULL_STDIN  $
     ELSE                SPAWN, gpcmd, /NULL_STDIN
     FILE_DELETE, file_gp
  ENDIF
END
