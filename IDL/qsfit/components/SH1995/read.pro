fwhm = 3e3 ;;km/s
tbe = 1.
bac_norm = 0.00025
density = ' 1.000E+02'
;Te =   5   &   f = 'r1b0005.d'
;Te =  10   &   f = 'r1b0010.d'
;Te =  30   &   f = 'r1b0030.d'
;Te =  50   &   f = 'r1b0050.d'
;Te =  75   &   f = 'r1b0075.d'
;Te = 100   &   f = 'r1b0100.d'
;Te = 125   &   f = 'r1b0125.d'
Te = 150   &   f = 'r1b0150.d'
;Te = 200   &   f = 'r1b0200.d'
;Te = 300   &   f = 'r1b0300.d'
Te *= 100.

c = gpc()
ryd = c.r_inf/c.ev ;;eV
ryd = c.c / (c.r_inf / c.h) * 1.d8 ;;Angstrom
edge = ryd*4. ;;Balmer edge


;;Pseudo-continuum
SPAWN, "cat " + f + " | ./convert.pl | grep E_NU | grep 'NE="+density+"' | perl -pe 's/ *Z=.*CASE=B//g; s/E_NU=//g' > tmp"

wave = LIST()
flux = LIST()
OPENR, lun, 'tmp', /get_lun
WHILE (~EOF(lun)) DO BEGIN
   l = ''
   READF, lun, l
   l = STRSPLIT(l, ' ', /extract)
   upper = LONG(l[0])

   FOR lower=1, upper-1 DO BEGIN
      gprint, lower, upper, FLOAT(l[lower]), ryd / (1.d/lower^2.d - 1.d/upper^2.d)
      wave.add, ryd / (1.d/lower^2.d - 1.d/upper^2.d)
      flux.add, FLOAT(l[lower])
   ENDFOR
ENDWHILE
FREE_LUN, lun

wave = wave.toArray()
flux = flux.toArray()
flux /= MAX(flux)


x = gloggen(gminmax(wave) * [0.1, 2], 10000)
x = gloggen(MIN(wave) * 0.1, 7000, 10000)
y = DOUBLE(x*0.)

FOR i=0, gn(wave)-1 DO BEGIN
   s = (fwhm / 3.e5) * wave[i] / 2.35
   exp = ((x-wave[i]) / s)^2. / 2.
   y += flux[i] * EXP( -exp ) / s
ENDFOR
;y[WHERE(x LT edge)] = 0.


;;Balmer continuum
l = x*1.e-8
b = 2 * c.h * c.c^2.d / (l^5.d) / (EXP(c.h*c.c / (l * c.k * Te)) - 1.d)
bac = b * (1.d - EXP(-(tbe * (x/edge)^3.d)))
bac[WHERE(x GE edge)] = 0.
bac /= MAX(bac)
bac *= bac_norm
gplot, x, bac

int = INT_TABULATED(x, bac)

tmp = bac*0.d
FOR i=0, gn(x)-1 DO BEGIN
   s = (fwhm / 3.e5) * x[i] / 2.35
   exp = ((x-x[i]) / s)^2. / 2.
   tmp += bac[i] * EXP( -exp ) / s
ENDFOR
bac = TEMPORARY(tmp)
bac *= int / INT_TABULATED(x, bac)
gplot, x, bac, /overpl




gplot, x, bac + y, pl='w l notit', $
       xr=[2000, 6000], $
       yr=[0, MAX([(y+bac)[WHERE(ABS(x-4861) LT 20)], MAX(bac)])], $
       titl='Te='+gn2s(Te) + ", Ne="+density+", t="+gn2s(tbe) + ', FWHM='+gn2s(fwhm) + ' km/s, BaC='+gn2s(bac_norm)

END

