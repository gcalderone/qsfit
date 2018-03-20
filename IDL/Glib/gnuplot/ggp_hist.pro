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
;  ggp_hist
;
;PURPOSE:
;  Compute a 1D or 2D histogram of input data (using ghist), prepare
;  the data set to be plotted (using ggp_data) and return the
;  appropriate argument to be passed to ggp_plot.
;
;PARAMETERS:
;  INDATA  (input, array of numbers)
;    Input array arranged as [#data] (1D case) or [#data, 2] (2D case).
;
;  BIN  (output, structure)
;    The edges of the bin (as returned by ghist).
;
;  HIST (output, array of integers)
;    The computed histogram (as returned by ghist).
;
;  /CLEAR (keyword)
;    Call ggp_clear before any other operation.
;
;  NBIN=, BINSIZE=, RANGE=, LOG=
;    All these keywords are passed to ghist (see ghist documentation).
;
;  LEVELS= (optional input, array of numbers, used only in the 2D case)
;    Contour levels for 2D histograms containing the given fraction of
;    counts.  The numbers must be in the range 0:1.
;    E.g. levels=[0.683, 0.955] will draw a contour map with two
;    colors: the darker and inner one will contain 68.3 of cthe
;    counts, the lighter and outer one will contain 95.5% of the
;    counts.  Default values are [0.5, 0.683, 0.955]
;
;  CLEVELS= (optional input, array of at most 3 numbers, used only in the 2D case)
;    Specify at most 3 levels where the color palette should change.
;    The last palette (or the only one if CLEVELS= is not given) is
;    the gray scale.  If 1, 2 or 3 levels are specified through the
;    CLEVELS= keyword a red, green and blue palette are added
;    respectively.
;
;  OUTLEVELS= (output, array of numbers)
;    The actual levels in the histogram corresponding to the levels
;    given as input.  This array will have the same number of elements
;    as LEVELS.
;
;  /LEGEND= (keyword, used only in the 2D case)
;    If given the legend labels will be added to the returned
;    structure.
;
;  /SPARSE (keyword)
;    Display individual points falling outside the outer contour
;    level.
;
FUNCTION ggp_hist        $
   , inData, bin, hist   $
   , CLEAR=clear         $
   , NBIN=nbin, BINSIZE=bs, RANGE=range, LOG=log $
   , LEVELS=levels, CLEVELS=clevels, OUTLEVELS=outlevels $
   , SPARSE=sparse, LEGEND=legend
  COMPILE_OPT IDL2
  ON_ERROR, !glib.on_error

  IF (KEYWORD_SET(clear)) THEN ggp_clear

  ;;Compute histogram
  hist = ghist(inData, bin, NBIN=nbin, BINSIZE=bs, RANGE=range, LOG=log, REVID=revid)
  dim = gn(gdim(hist))

  plotOpt = []

  ;;1D ---------------------------------------------------------------
  IF (dim EQ 1) THEN BEGIN
     IF (KEYWORD_SET(log)) THEN BEGIN
        ggp_cmd, 'set logscale x'
        ggp_cmd, 'set mxtics 10'
        ggp_cmd, 'set format x "10^{%L}"'
     ENDIF

     ggp_data, bin.mid, hist, getname=name
     plotOpt = [plotOpt, ggp_plot_struct()]
     plotOpt[-1].name = name
     plotOpt[-1].w = 'histeps'
  ENDIF


  ;;2D ---------------------------------------------------------------
  IF (dim EQ 2) THEN BEGIN
     ;;2D histograms require solid fill
     ggp_cmd, 'set style fill solid'
     IF (log[0]) THEN BEGIN
        ggp_cmd, 'set logscale x'
        ggp_cmd, 'set mxtics 10'
        ggp_cmd, 'set format x "10^{%L}"'
     ENDIF
     IF (log[1]) THEN BEGIN
        ggp_cmd, 'set logscale y'
        ggp_cmd, 'set mytics 10'
        ggp_cmd, 'set format y "10^{%L}"'
     ENDIF

     ;;Contour levels: 0.683 means that 68.3% of the points are
     ;;contained within the corresponding contour.  The smaller the
     ;;value the inner the contour.
     IF (~KEYWORD_SET(levels)) THEN $
        levels = [0.5, 0.683, 0.955]

     ;;Compute levels
     outlevels = FLTARR(gn(levels))

     l = hist[SORT(hist)]
     tmp = TOTAL(l, /cumul)
     tmp /= tmp[-1]

     FOR i=0, gn(levels)-1 DO BEGIN
        dummy = MIN(ABS(tmp - (1-levels[i])), j)
        outlevels[i] = l[j]
     ENDFOR

     ;;Gray scale
     col = LONG(ROUND(ggen(50, 200, gn(levels))))

     ;;Palette
     R0 = 256l^2l
     G0 = 256l
     B0 = 1l

     R1 = 0 * R0
     G1 = 0 * G0
     B1 = 1 * B0

     R2 = 1 * R0
     G2 = 0 * G0
     B2 = 0 * B0

     R3 = 0 * R0
     G3 = 1 * G0
     B3 = 0 * B0
     
     CASE (gn(clevels)) OF 
        0: col = col*R0 + col*G0 + col*B0 ;;gray
        1: BEGIN
           IF (gsearch(levels GT clevels[0]                         , i)) THEN col[i] = col[i]*R0 + col[i]*G0 + col[i]*B0 ;;gray
           IF (gsearch(levels LE clevels[0]                         , i)) THEN col[i] = col[i]*R1 + col[i]*G1 + col[i]*B1 
        END
        2: BEGIN
           IF (gsearch(levels GT clevels[1]                         , i)) THEN col[i] = col[i]*R0 + col[i]*G0 + col[i]*B0 ;;gray
           IF (gsearch(levels LE clevels[1] AND levels GT clevels[0], i)) THEN col[i] = col[i]*R1 + col[i]*G1 + col[i]*B1
           IF (gsearch(levels LE clevels[0]                         , i)) THEN col[i] = col[i]*R2 + col[i]*G2 + col[i]*B2
        END
        3: BEGIN
           IF (gsearch(levels GT clevels[2]                         , i)) THEN col[i] = col[i]*R0 + col[i]*G0 + col[i]*B0 ;;gray
           IF (gsearch(levels LE clevels[2] AND levels GT clevels[1], i)) THEN col[i] = col[i]*R1 + col[i]*G1 + col[i]*B1
           IF (gsearch(levels LE clevels[1] AND levels GT clevels[0], i)) THEN col[i] = col[i]*R2 + col[i]*G2 + col[i]*B2
           IF (gsearch(levels LE clevels[0]                         , i)) THEN col[i] = col[i]*R3 + col[i]*G3 + col[i]*B3
        END
        ELSE: MESSAGE, 'CLEVELS array length must be <= 3'
     ENDCASE

     data_low  = ggp_prepare_data({x: bin.(0).low , y: bin.(1).low , h: hist})
     data_high = ggp_prepare_data({x: bin.(0).high, y: bin.(1).high, h: hist})
     data_mid  = data_low
     data_mid.(0) = (data_low.(0) + data_high.(0)) / 2.
     data_mid.(1) = (data_low.(1) + data_high.(1)) / 2.


     ;;Start from the larger contour to show inner ones on top of it
     FOR i=0, gn(outlevels)-1 DO BEGIN
        IF (i EQ 0) THEN $
           j = WHERE(hist GE outlevels[i]) $
        ELSE $
           j = WHERE(hist GE outlevels[i]  AND   $
                     hist LT outlevels[i-1])

        IF (j[0] NE -1) THEN BEGIN
           ggp_data, data_mid [j].(0), data_mid [j].(1), $
                     data_low [j].(0), data_high[j].(0), $
                     data_low [j].(1s), data_high[j].(1), $
                     getname=name
           plotOpt = [plotOpt, ggp_plot_struct()]
           plotOpt[-1].name = name
           plotOpt[-1].w    = 'boxxyerrorbars'
           plotOpt[-1].lty  = '1'
           plotOpt[-1].lc   = 'rgb "#' + STRJOIN(gn2hex(col[i]), '') + '"'
           IF (KEYWORD_SET(legend)) THEN $
              plotOpt[-1].t = gn2s(levels[i]*100, '%5.1f\%')
        ENDIF
     ENDFOR


     ;;Sparse ----------------------------------------------------------
     IF (KEYWORD_SET(sparse)) THEN BEGIN
        i = WHERE(hist[revid] LT MIN(outlevels))

        sparse_x = []
        sparse_y = []

        IF (i[0] NE -1) THEN BEGIN
           sparse_x = inData[i, 0]
           sparse_y = inData[i, 1]

           IF (KEYWORD_SET(range)) THEN BEGIN
              IF (gsearch(sparse_x GE range[0,0]      AND  $
                          sparse_x LE range[1,0]      AND  $
                          sparse_y GE range[0,1]      AND  $
                          sparse_y LE range[1,1], i)) THEN BEGIN
                 sparse_x = sparse_x[i]
                 sparse_y = sparse_y[i]
              ENDIF $
              ELSE  BEGIN
                 sparse_x = []
                 sparse_y = []
              ENDELSE
           ENDIF
        ENDIF

        IF (gn(sparse_x) GT 0) THEN BEGIN
           ggp_data, sparse_x, sparse_y, getname=name

           plotOpt = [plotOpt, ggp_plot_struct()]
           plotOpt[-1].name  = name
           plotOpt[-1].w     = 'dots'
           plotOpt[-1].lc    = 'rgb "black"'
        ENDIF $
        ELSE  $
           PRINT, 'All points fell in the histogram, no separate point displayed'
     ENDIF
  ENDIF

  RETURN, plotOpt
END
