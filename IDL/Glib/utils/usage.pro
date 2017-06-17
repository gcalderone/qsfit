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
PRO usage_ghist
  ;;----------- 1D HISTO -----------
  x = ggen(0, 10, 1000)
  
  ;;Compute histogram using both ghist and HISTOGRAM
  hist  = ghist(x, bin, NBIN=5)
  hist2 = HISTOGRAM(x, LOCATIONS=bin2, NBIN=5)

  ;;The following must all be 1000
  PRINT, gn(x), TOTAL(hist), TOTAL(hist2)

  ;;Compare left edge of the bins
  gprint, bin.low, bin2

  ;;Compare histograms
  gprint, hist, hist2

  ;;Compare plots
  PLOT , bin.low, hist , PSYM=10, YRANGE=[0, MAX([hist, hist2])], xr=[-0.5, 10.5], xst=1, yst=3
  OPLOT, bin2   , hist2, PSYM=10, COLOR=200
  OPLOT, bin.mid, hist , PSYM=2 , SYMSIZE=2
  OPLOT, bin2   , hist2, PSYM=2 , SYMSIZE=2, COLOR=200


  ;;Compute histogram of a log-uniform distribution
  x = gloggen(1, 10, 1000)  
  hist  = ghist(x, bin, NBIN=5, /log)
  hist2 = HISTOGRAM(ALOG10(x), LOCATIONS=bin2, NBIN=5)
  bin2 = 10.d^bin2

  ;;The following must all be 1000
  PRINT, gn(x), TOTAL(hist), TOTAL(hist2)

  ;;Compare left edge of the bins
  gprint, bin.low, bin2

  ;;Compare histograms
  gprint, hist, hist2

  ;;Compare plots
  PLOT , bin.low, hist , PSYM=10, YRANGE=[0, MAX([hist, hist2])], xr=[0.9, 12], xst=1, yst=3, /xlog
  OPLOT, bin2   , hist2, PSYM=10, COLOR=200
  OPLOT, bin.mid, hist , PSYM=2 , SYMSIZE=2
  OPLOT, bin2   , hist2, PSYM=2 , SYMSIZE=2, COLOR=200


  ;;Generate and plot the histogram of a normal distribution
  x = RANDOMN(seed, 1000)
  hist = ghist(x, bin, revid=revid, nbin=20)
  PLOT, bin.mid, hist, PSYM=10, XR=[-3.5, 3.5]

  ;;Use REVID to connect input data and output bins
  id = 14
  PRINT, ''
  PRINT, 'Input data ', x[id]
  PRINT, '  has been accounted for in bin: ', revid[id]
  PRINT, '  whose limits are:', bin[revid[id]].low, bin[revid[id]].high 
  PRINT, ''

  id = 18
  PRINT, 'Bin ', gn2s(id), ' has limits:', bin[id].low, bin[id].high
  PRINT, '  and ', gn2s(hist[id]), ' counts'
  IF (hist[id] GT 0) THEN $
     PRINT, '  input data falling in this bin are: ', x[WHERE(revid EQ id)]
  PRINT, ''


  ;;----------- 2D HISTO -----------
  ;;Generate a 2D normal distribution and plot the histogram as a contour
  x = [[RANDOMN(seed, 1000)], [RANDOMN(seed, 1000)]]
  hist = ghist(x, bin, nbin=[10, 11])
  CONTOUR, hist, bin.(0).mid, bin.(1).mid, nlev=6, XR=[-3.5, 3.5], YR=[-3.5, 3.5]

  ;;Compute histogram using logarithmic values and inserting a few NaN
  x = [[RANDOMN(seed, 1000)], [RANDOMN(seed, 1000)]]
  x = x - MIN(x) + 2
  x[10,*] = gnan() 
  x[20,*] = gnan() 
  hist = ghist(x, bin, nbin=[10,11], log=[1,1], multi_revid=revid)
  CONTOUR, hist, bin.(0).mid, bin.(1).mid, nlev=6, /xlog, /ylog

  ;;Use REVID to connect input data and output bins
  id = 14
  PRINT, 'Input data ', REFORM(x[id,*])
  PRINT, '  has been accounted for in bin: ', REFORM(revid[id,*])
  binX = bin.(0)[revid[id,0]]
  binY = bin.(1)[revid[id,1]]
  PRINT, '  whose limits are: (X) ', binX.low, binX.high 
  PRINT, '                    (Y) ', binY.low, binY.high 
  PRINT, ''

  idX = 4
  idY = 5
  PRINT, 'Bin ', gn2s(idX), ',', gn2s(idY)
  PRINT, '  has limits: (X) ', bin.(0)[idX].low, bin.(0)[idX].high
  PRINT, '              (Y) ', bin.(1)[idY].low, bin.(1)[idY].high
  PRINT, '  and ', gn2s(hist[idX, idY]), ' counts'
  IF (hist[idX, idY] GT 0) THEN BEGIN
     PRINT, '  input data falling in this bin are: '
     i = WHERE(revid[*,0] EQ idX  AND  revid[*,1] EQ idY)
     gprint, x[i,0], x[i,1]
  ENDIF

  ;;Search for data not assigned to bins
  i = WHERE(revid[*,0] EQ -1)
  IF (i[0] NE -1) THEN BEGIN
     PRINT, 'The following data:'
     gprint, i, x[i,0], x[i,1]
     PRINT, ' has not been accounted in any bin'
  ENDIF $
  ELSE $
     PRINT, 'All data have been accounted in output histogram'


  ;;----------- 3D HISTO -----------
  ;;Generate the histogram of a 2D normal distribution
  x = [[RANDOMN(seed, 1000)], [RANDOMN(seed, 1000)], [RANDOMN(seed, 1000)]]
  hist = ghist(x, bin, nbin=[10, 11, 12], multi_revid=revid)

  ;;Use REVID to connect input data and output bins
  id = 14
  PRINT, 'Input data ', REFORM(x[id,*])
  PRINT, '  has been accounted for in bin: ', REFORM(revid[id,*])
  binX = bin.(0)[revid[id,0]]
  binY = bin.(1)[revid[id,1]]
  binZ = bin.(2)[revid[id,2]]
  PRINT, '  whose limits are: (X) ', binX.low, binX.high 
  PRINT, '                    (Y) ', binY.low, binY.high 
  PRINT, '                    (Z) ', binZ.low, binZ.high 
  PRINT, ''

  idX = 4
  idY = 5
  idZ = 6
  PRINT, 'Bin ', gn2s(idX), ',', gn2s(idY), ',', gn2s(idZ)
  PRINT, '  has limits: (X) ', bin.(0)[idX].low, bin.(0)[idX].high
  PRINT, '              (Y) ', bin.(1)[idY].low, bin.(1)[idY].high
  PRINT, '              (Z) ', bin.(2)[idZ].low, bin.(2)[idZ].high
  PRINT, '  and ', gn2s(hist[idX, idY, idZ]), ' counts'
  IF (hist[idX, idY, idZ] GT 0) THEN BEGIN
     PRINT, '  input data falling in this bin are: '
     i = WHERE(revid[*,0] EQ idX  AND  revid[*,1] EQ idY  AND  revid[*,2] EQ idZ)
     gprint, x[i,0], x[i,1], x[i,2]
  ENDIF
END



;=====================================================================
PRO usage_gstru_sub
  ;;Create a simple structure
  str = {a: 'A string', b: 1, c: !PI}
  
  ;;Replicate the structure into an array
  str = REPLICATE(str, 10)
  str.b = ggen(1, 10, 10)

  ;;Drop the "c" field and print using gps
  gps, gstru_sub(str, keep=['a', 'b'])

  ;;Drop the "c" field, rename the "a" field (but leave the "b" field
  ;;its name) and print using gps.
  gps, gstru_sub(str, keep=['a', 'b'], rename=['renamed', ''])

  ;;Alternaltivaly, we can directly drop the "c" field
  gps, gstru_sub(str, drop='c')
END



;=====================================================================
PRO usage_gps, _EXTRA=e
  str = { a: 'aaa', b: 1, c: 3.14, d: FINDGEN(5)+0.5, e: LINDGEN(6), f:['one', 'two'], g:STRARR(10) }
  gps, str, _extra=e
  PRINT


  str = REPLICATE(str, 11)
  gps, str, _extra=e
  PRINT


  gps, str, _extra=e, skip=[0,1,4]
END



;=====================================================================
PRO usage_ggen
  PRINT, ggen( 1, 10 , 5)
  PRINT, ggen([1, 10], 5)
END

;=====================================================================
PRO gloggen_usage
  ;;The logarithm of the sequence is evenly spaced.
  PRINT, ALOG10(gloggen( 1, 10 , 5))
  PRINT, ALOG10(gloggen([1, 10], 5))
END

;=====================================================================
PRO ggen_delta_usage
  ;;The sequence is evenly spaced.
  PRINT, ggen_delta( 1, 10 , 2.25)
  PRINT, ggen_delta([1, 10], 2.25)
END



;=====================================================================
PRO usage_gindices_array
  a=[2,3,4,5]
  i=110
  PRINT, gindices_array(a, ARRAY_INDICES(a, i, /DIM), /DIM)
END


PRO usage_gstru_insert
  s = []

  s = gstru_insert(s, 'a', 'a')
  gps, s

  s = gstru_insert(s, 'b', 'b')
  gps, s

  s = gstru_insert(s, 'c', 'c', 0)
  gps, s

  s = gstru_insert(s, 'd', 'd', N_TAGS(s))
  gps, s

  s = REPLICATE(s, 3)

  s = gstru_insert(s, 'aa', 'a')
  gps, s

  s = gstru_insert(s, 'bb', 'b')
  gps, s

  s = gstru_insert(s, 'cc', 'c', 0)
  gps, s

  s = gstru_insert(s, 'dd', 'd', N_TAGS(s))
  gps, s
  
  array = FINDGEN(3)

  s = gstru_insert(s, 'ff', array)
  gps, s

  s = gstru_insert(s, 'gg', array, 0)
  gps, s

  s = gstru_insert(s, 'hh', array, N_TAGS(s))
  gps, s

  s = gstru_insert(s, 'ii', array, /as_array)
  gps, s

  s = gstru_insert(s, 'jj', array, 0, /as_array)
  gps, s

  s = gstru_insert(s, 'kk', array, N_TAGS(s), /as_array)
  gps, s

  s = gstru_insert(s, 'll', array, 3, /as_array)
  gps, s
END
