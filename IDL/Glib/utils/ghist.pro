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
;  ghist
;
;PURPOSE:
;  Compute histograms of any-dimensional data arrays.
;
;PARAMETERS:
;  _X  (input, array of any type except struct)
;    Input data  arranged as [#data] (1D case) or [#data, #dimensions]
;    (multi-dimensional case)
;
;  BIN (output, structure)
;    Location of bins
;
;  REVID= (output, array of size [#data])
;    Index of the bin accounting for each input data.
;
;  MULTI_REVID= (output, array of size [#data, #dimensions])
;    Index of the bin accounting for each input data.  This is similar
;    to REVID=, but the index are now multi dimensional subscripts.
;    In the 1D case this is exactly the same as REVID=.
;
;  NBIN= (optional input/output, array of integers arranged as [#dimensions])
;    Number of bins in each dimension.
;
;  BINSIZE= (optional input/output, array of numbers arranged as [#dimensions])
;    Bin size in each dimension.
;
;  RANGE= (optional input/output, array of numbers arranged as [2, #dimensions])
;    Range to consider (minimum and maximum) for each dimension.
;
;  LOG= (optional input, an array of either 0 or 1 arranged as [#dimensions])
;    Boolean value to indicate whether the histogram should be
;    computed on the input values (log=0) or on the logarithmic values
;    (log=1).
;
;RETURN VALUE:
;  Array of counts arranged as [#NBIN] (1D case), [#NBIN1, #NBIN2] (2D case), etc...
;
;NOTES:
;  In the 1D case this function is supposed to replace the HISTOGRAM
;  function, the main difference being the way the domain is divided
;  into bins. The usage_ghist procedure shows the difference.
;
FUNCTION ghist        $
   , _x, bin          $
   , REVID=revid, MULTI_REVID=multi_revid $
   , NBIN=_nbin, BINSIZE=_bs, RANGE=_range, LOG=log
  COMPILE_OPT IDL2
  ON_ERROR, !glib.on_error

  ;How many dimensions in input data?
  ndim = [gsize(_x), 1]
  nn   = ndim[0]
  ndim = ndim[1]

  ;Local copies
  x = DOUBLE(REFORM(_x))

  range = []
  IF (gn(_range) NE 0) THEN BEGIN
     gassert, gn(_range) EQ 2*ndim
     range = DOUBLE(_range)
  ENDIF

  bs = []
  IF (gn(_bs) NE 0) THEN BEGIN
     gassert, gn(_bs) EQ ndim
     gassert, MIN(_bs) GT 0
     bs = DOUBLE(_bs)
  ENDIF

  nbin = []
  IF (gn(_nbin) NE 0) THEN BEGIN
     gassert, gn(_nbin) EQ ndim
     gassert, MIN(_nbin) GT 1
     nbin = LONG(_nbin)
  ENDIF

  ;;Use logarithmic values ?
  IF (gn(log) NE 0) THEN gassert, gn(log) EQ ndim $
  ELSE log = REPLICATE(0b, ndim)
  FOR i=0, ndim-1 DO $
     IF (log[i]) THEN x[*,i] = ALOG10( x[*,i] )

  ;;if user did not provided the range evaluate min/max
  IF (gn(range) EQ 0) THEN BEGIN
     range = FLTARR(2, ndim)
     FOR i=0, ndim-1 DO BEGIN
        min = MIN(x[*,i], MAX=max, /nan)
        gassert, min LT max

        ;;The range is slightly enlarged otherwise the list of valid
        ;;points (see "valid" below) may consider invalid those
        ;;numbers at the edge of the range
        range[*,i] = [min,max] + [-1,1] * (max-min) / 1000.
     ENDFOR

     range = REFORM(range)
  ENDIF $
  ELSE BEGIN
     ;;The range provided by user is in linear scale.  If required
     ;;compute logarithms
     FOR i=0, ndim-1 DO $
        IF (log[i]) THEN range[*,i] = ALOG10( range[*,i] )
  ENDELSE

  ;;Data outside the range are treated as NaN
  FOR i=0, ndim-1 DO BEGIN
     j = WHERE((x[*,i] LT range[0,i])   OR   $
               (x[*,i] GT range[1,i]))
     IF (j[0] NE -1) THEN x[j,i] = gnan()
  ENDFOR

  ;Identify non-NaN values (on all dimensions)
  IF (ndim GT 1) THEN valid = WHERE(TOTAL(FINITE(x), 2) EQ ndim) $
  ELSE                valid = WHERE(      FINITE(x)            )
  IF (valid[0] EQ -1) THEN $
     MESSAGE, 'There is no data within the range'

  ;;If NBIN is given compute the bin size
  IF (KEYWORD_SET(nbin)) THEN BEGIN
     bs = FLTARR(ndim)
     FOR i=0, ndim-1 DO $
        bs[i] = (range[1,i]-range[0,i]) / nbin[i]
  ENDIF $
  ELSE BEGIN
     ;;If neither the BS or NBIN are given compute a default bin size
     IF (~KEYWORD_SET(bs)) THEN BEGIN
        ;;http://www.fmrib.ox.ac.uk/analysis/techrep/tr00mj2/tr00mj2/node24.html
        bs = FLTARR(ndim)
        FOR i=0, ndim-1 DO $
           bs[i] = 3.49 * STDDEV(x[valid,i]) / (gn(valid)^(1./3))

        gassert, TOTAL(FINITE(bs)) EQ ndim
        gassert, MIN(bs) GT 0
     ENDIF

     ;;Compute NBIN
     nbin = LONARR(ndim)
     FOR i=0, ndim-1 DO $
        nbin[i] = CEIL((range[1,i]-range[0,i]) / bs[i])
     gassert, nbin GT 0
  ENDELSE
  IF (ndim EQ 1) THEN bs = bs[0]


  ;;Compute the histogram
  index = LONG(x[valid,*])
  FOR i=0, ndim-1 DO BEGIN
     ;Compute index in the final histogram for current dimension
     index[*,i] = FLOOR((x[valid,i] - range[0,i]) / bs[i])

     ;;Account for points on left/right edges of first/last bin
     j = WHERE(x[valid,i] EQ range[0,i])
     IF (j[0] NE -1) THEN index[j,i] = 0

     j = WHERE(x[valid,i] EQ range[1,i])
     IF (j[0] NE -1) THEN index[j,i] = nbin[i]-1

     ;Check for values outside bins
     j = WHERE(index[*,i] LT 0)
     IF (j[0] NE -1) THEN index[j,i] = 0

     j = WHERE(index[*,i] GE nbin[i])
     IF (j[0] NE -1) THEN index[j,i] = nbin[i]-1
  ENDFOR


  ;;Prepare output variables
  hist  = REPLICATE(0, nbin)
  revid = REPLICATE(-1l, nn)

  ;;Fill output variables
  FOR i=0, gn(valid)-1 DO BEGIN
     j = REFORM(index[i,*])
     IF (ndim GT 1) THEN j = gindices_array(nbin, /dim, j)
     gassert, j GE 0

     ;;Increment counts in histogram
     hist[j] += 1

     ;;valid[i]-th point has been counted in the j-th bin in the histogram
     revid[valid[i]] = j
  ENDFOR

  ;;Bin locations are returned as a structure with as many fields as
  ;;the number of dimensions.  Each field is an array of structures
  ;;with two fields containing the low and high limit of each bin.
  bin = []
  FOR i=0, ndim-1 DO BEGIN
     bb = REPLICATE({low: 0.d, mid: 0.d, high: 0.d}, nbin[i])
     bb.low  = FINDGEN(nbin[i]) * bs[i] + range[0,i]
     bb.mid  = bb.low + bs[i]/2.
     bb.high = bb.low + bs[i]
     bin = CREATE_STRUCT(bin, 'dim' + gn2s(i), bb)
  ENDFOR

  ;;Re-arrange revid for multi-dimensional histogram
  multi_revid = revid
  IF (ndim GT 1) THEN BEGIN
     tmp = LONARR(gsize(x))
     tmp[*] = -1
     tmp[valid,*] = TRANSPOSE(ARRAY_INDICES(nbin, /dime, revid[valid]))
     multi_revid = TEMPORARY(tmp)
  ENDIF


  ;;Check output (to be disabled if performance is affected...)
  IF (1) THEN BEGIN
     gassert, TOTAL(hist) EQ gn(valid)
     gassert, MIN(revid[valid]) GE 0

     nonValid = REPLICATE(1b, N_ELEMENTS(revid))
     nonValid[valid] = 0
     nonValid = WHERE(nonValid)
     IF (nonValid[0] NE -1) THEN BEGIN
        gassert, revid[nonValid] EQ -1
     ENDIF

     ;;Break revid indices into multidimensional index
     FOR i=0, ndim-1 DO BEGIN
        IF (MAX(ABS( x[valid,i] - bin.(i)[multi_revid[valid,i]].mid)) GT 1.001 * bs[i]/2.) THEN $
           MESSAGE, 'Unexpected behaviour'
     ENDFOR
  ENDIF


  ;;If logarithms has been used switch back to linear scale
  FOR i=0, ndim-1 DO BEGIN
     IF (log[i]) THEN BEGIN
        bin.(i).low  = 10.d^(bin.(i).low)
        bin.(i).mid  = 10.d^(bin.(i).mid)
        bin.(i).high = 10.d^(bin.(i).high)
        range[*,i]   = 10.d^(range[*,i])
     ENDIF
  ENDFOR

  ;;For 1D histogram there is no need for a nested structure, hence we
  ;;drop one level.
  IF (ndim EQ 1) THEN BEGIN
     bin = bin.(0)
     bs = bs[0]
     nbin = nbin[0]
  ENDIF


  ;;If optional inputs are empty variables we return currently used values
  IF (gn(_range) EQ 0) THEN _range = range
  IF (gn(_bs   ) EQ 0) THEN _bs    = bs
  IF (gn(_nbin ) EQ 0) THEN _nbin  = nbin

  RETURN, hist
END
