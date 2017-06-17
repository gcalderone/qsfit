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
PRO usage_ggp
  ;;Generate some data
  x = findgen(10)

  ;;Initialize GGP
  ggp_clear

  ;;Add a dataset with name foo
  ggp_data, name='foo', x, x

  ;;Plot command for dataset foo
  ggp_plot, '$foo w l'

  ;;Add a dataset with a default assigned name (passed back in "name")
  ggp_data, getname=name, x, x^0.5

  ;;Plot the dataset
  ggp_plot, name + ' w l'

  ;;Produce the plot in a "persistent" gnuplot session.  Temporary
  ;;file is deleted
  ggp

  ;;Save the gnuplot file in "aaa.gp" 
  ggp, gp='aaa.gp'
  
  ;;Save current GGP state in aaa.dat
  saved_state = ggp_get_state()
  SAVE, file='aaa.dat', saved_state

  ;;Export to ccc.pdf
  ggp, term='pdf', output='ccc.pdf'

  ;;Now close and reopen IDL session, restore aaa.dat and plot again
  RESTORE, 'aaa.dat'
  ggp_set_state, saved_state
  ggp

  z = DIST(200)
  x = FINDGEN(200)
  y = FINDGEN(200)

  ggp_clear
  ggp_data, x, y, z, plot='w l pal'
  ggp
END





;=====================================================================
PRO usage_ggp_hist
  seed = SYSTIME(1)
  
  tmp = LINDGEN(10)
  x = []
  FOR i=0, 4 DO $
     x = [x, tmp[i:gn(tmp)-i-1]]
  x += 0.5
  
  ggp_plot, ggp_hist(/clear, x, binsize=1)
  ggp
  
  
  ggp_plot, ggp_hist(/clear, 10.d^x, /log, binsize=1)
  ggp
  
  
  x = RANDOMN(seed, 1000)
  ggp_plot, ggp_hist(/clear, x)
  ggp
  
  p = ggp_hist(/clear, x)
  p.t = 'My title'
  ggp_plot, p
  ggp
  

  x = [[RANDOMN(seed, 10000)], [RANDOMN(seed, 10000)]]
  ggp_plot, ggp_hist(/clear, x, /sparse)
  ggp

  p = ggp_hist(/clear, x, /sparse)
  ggp_plot, p[0:-2]
  ggp

  
  p[-1].w  = 'points'
  p[-1].pt = ''
  p[-1].lc = 'rgb "red"'
  ggp_plot, p[-1]  
  ggp
END




;=====================================================================
PRO usage_ggp_prepare_data
  gps, row=0, skip=kb, ggp_prepare_data(keyb=kb, 1)
  gps, row=0, skip=kb, ggp_prepare_data(keyb=kb, 1, 3)
  gps, row=0, skip=kb, ggp_prepare_data(keyb=kb, [10, 1], [3,4])

  x = FINDGEN(5)+0.5
  gps, row=0, skip=kb, ggp_prepare_data(keyb=kb, x)
  gps, row=0, skip=kb, ggp_prepare_data(keyb=kb,    '_' + gn2s(x))
  gps, row=0, skip=kb, ggp_prepare_data(keyb=kb, x, '_' + gn2s(x))

  x = FINDGEN(2, 3)+0.5
  gps, row=0, skip=kb, ggp_prepare_data(keyb=kb, x)
  gps, row=0, skip=kb, ggp_prepare_data(keyb=kb,    '_' + gn2s(x))
  gps, row=0, skip=kb, ggp_prepare_data(keyb=kb, x, '_' + gn2s(x))
  gps, row=0, skip=kb, ggp_prepare_data(keyb=kb, FINDGEN(2), FINDGEN(3), x, '_' + gn2s(x))

  x = FINDGEN(2, 3, 4)+0.5
  gps, row=0, skip=kb, ggp_prepare_data(keyb=kb, x)
  gps, row=0, skip=kb, ggp_prepare_data(keyb=kb,    '_' + gn2s(x))
  gps, row=0, skip=kb, ggp_prepare_data(keyb=kb, x, '_' + gn2s(x))
  gps, row=0, skip=kb, ggp_prepare_data(keyb=kb, FINDGEN(2), FINDGEN(3), FINDGEN(4), x, '_' + gn2s(x))

  ;;NOTE: the following is executed without error, but will scramble
  ;;the data since the dimensions of independent variables are
  ;;inverted
  gps, row=0, skip=kb, ggp_prepare_data(keyb=kb, FINDGEN(4), FINDGEN(3), FINDGEN(2), x, '_' + gn2s(x))

  x = [1.1, 2.2, 3.3]
  y = [5.5, 6.6]
  z =[[0,1,2], [3, 4, 5]]
  gps, row=0, skip=kb, ggp_prepare_data(keyb=kb, x, y, z)
END
