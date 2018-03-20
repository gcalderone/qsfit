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
;  ggp_plot_struct
;
;PURPOSE:
;  Returns the template of a structure containing all the most
;  commonly used plotting options.  The content of this structure may
;  be modified by the user and given to ggp_plot as parameter.
;
;PARAMETERS:
;  NONE
;
;RETURN VALUE:
;  The template of the structure with all fields empty.
;
FUNCTION ggp_plot_struct
  str = {name:    ''    $ ;;the data set name
         , u:     ''    $ ;;the "using" clause
         , w:     ''    $ ;;the "with" clause
         , t:     ''    $ ;;the "title" clause
         , lty:   ''    $ ;;the "linetype" clause
         , lw:    ''    $ ;;the "linewidth" clause
         , dt:    ''    $ ;;the "dashtype" clause
         , pt:    ''    $ ;;the "pointtype" clause
         , lc:    ''    } ;;the "linecolor" clause
  RETURN, str
END

