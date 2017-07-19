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
;  NORUN= (keyword)
;    If given avoid running gnuplot.
;
PRO ggp                             $
   , SPLOT=splot                    $
   , STDOUT=stdout, GP=file_gp      $
   , TERMINAL=term, OUTPUT=file_out $
   , NORUN=norun
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
  PRINTF, lun, 'set linetype  1 lc rgb "black"'
  PRINTF, lun, 'set linetype  2 lc rgb "red"'
  PRINTF, lun, 'set linetype  3 lc rgb "blue"'
  PRINTF, lun, 'set linetype  4 lc rgb "dark-green"'
  PRINTF, lun, 'set linetype  5 lc rgb "dark-violet"'
  PRINTF, lun, 'set linetype  6 lc rgb "dark-orange"'
  PRINTF, lun, 'set linetype cycle  6'


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

  FOR imulti=0, 100 DO BEGIN
     ;;Write commands
     FOR i=0, gn(ggp_cmd)-1 DO BEGIN
        IF (imulti NE ggp_cmd_m[i]) THEN CONTINUE
        PRINTF, lun, ggp_cmd[i]
     ENDFOR


     ;;Prepare plot commands
     plotCmd = LIST()
     IF (gn(ggp_plot) GT 0) THEN BEGIN
        FOR i=0, gn(ggp_plot)-1 DO BEGIN
           IF (imulti NE ggp_plot_m[i]) THEN CONTINUE
           plotCmd.add, ggp_plot[i]
        ENDFOR
     ENDIF

     ;;Write plot command
     IF (gn(plotCmd) GT 0) THEN BEGIN
        IF (KEYWORD_SET(splot)) THEN PRINTF, lun, 'splot \' $
        ELSE                         PRINTF, lun, 'plot  \'

        plotCmd = plotCmd.toArray()
        IF (gn(plotCmd) GT 1) THEN plotCmd[0:-2] += ', \'

        FOR i=0, gn(plotCmd)-1 DO $        
           PRINTF, lun, plotCmd[i]
     ENDIF
  ENDFOR

  IF (MAX(ggp_plot_m.toArray()) GT 0) THEN $
     PRINTF, lun, 'unset multiplot'
  

  IF (KEYWORD_SET(file_out)) THEN BEGIN
     PRINTF, lun, 'set output' ;;close output file
  ENDIF $
  ELSE BEGIN
     PRINTF, lun, '#set output'
  ENDELSE

  IF (~KEYWORD_SET(stdout)) THEN $
     FREE_LUN, lun

  ;;Spawn gnuplot process
  IF (KEYWORD_SET(norun)) THEN runGnuplot = 0
  IF (runGnuplot) THEN BEGIN
     IF (gisunix()) THEN BEGIN
        IF (term EQ 'qt') THEN BEGIN
           SPAWN, 'mktemp -u /tmp/idl.gp.XXXXXXXX', tmpFile
           SPAWN, 'cp ' + file_gp + ' ' + tmpFile
           ;;Special behaviour for the qt terminal
           ;gpcmd = 'xterm -geometry 30x5 -iconic -e "gnuplot ' + file_gp + ' -" &'
           gpcmd = '(export LD_PRELOAD=""; gnuplot ' + tmpFile + ' -e "pause mouse close"; rm -f ' + tmpFile + ') &'
           SPAWN, gpcmd
        ENDIF $
        ELSE BEGIN
           gpcmd = 'export LD_PRELOAD=""; gnuplot -persist "' + file_gp + '"' ;;default gnuplot invocation
           SPAWN, gpcmd, out, err
        ENDELSE
     ENDIF ELSE BEGIN
        gpcmd = 'gnuplot  -persist "' + file_gp + '"' ;;default gnuplot invocation
        SPAWN, gpcmd, /NULL_STDIN
     ENDELSE

     FILE_DELETE, file_gp
  ENDIF
END
