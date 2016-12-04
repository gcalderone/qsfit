PRO gfit_rebin, rebin, x, y, e
  COMPILE_OPT IDL2
  ON_ERROR, !glib.on_error

  IF (rebin LE 1) THEN RETURN

  k = 0
  FOR i=0, gn(x)-1, rebin DO BEGIN
     IF (i+rebin GE gn(x)) THEN BREAK
     j = INDGEN(rebin) + i
     x[k] = MEAN (x[j])

     CASE (N_PARAMS()) OF
        2:
        3: BEGIN
           y[k] = MEAN (y[j])
        END
        4: BEGIN
           y[k] = TOTAL(y[j] / e[j] ) / TOTAL(1./e[j])
           e[k] = SQRT(TOTAL(e[j]^2)) / rebin
        END
     ENDCASE
     k += 1
  ENDFOR
  
  x = x[0:k-1]
  IF (N_PARAMS() GE 3) THEN y = y[0:k-1]
  IF (N_PARAMS() GE 4) THEN e = e[0:k-1]
END
