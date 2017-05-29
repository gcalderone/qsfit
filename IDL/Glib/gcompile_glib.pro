; *******************************************************************
; Copyright (C) 2016,2017 Giorgio Calderone
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
;  gcompile_glib
;
;PURPOSE:
;  Initialize the 'glib' package.
;
;DEPENDENCIES:
;  gcompile (https://github.com/gcalderone/gcompile)
;
;PARAMETERS:
;  PATH (optional input, a scalar string)
;    Path to the glib directory.  If this parameter is not given 
;    the path where this routine is stored will be considered.
;
;USAGE:
;  To compile the 'glib' package you should either:
;
;  - copy the gcompile_glib.pro file in a directory listed in the
;    IDL_PATH environment variable and call:
;      gcompile_glib, 'PATH/TO/glib'
;
;  - copy the gcompile_glib.pro file in a directory NOT listed in the
;    IDL_PATH environment variable (say PATH/TO/glib), add:
;    the following line in one of your source files:
;      @PATH/TO/glib/gcompile_glib.pro
;    and then call:
;      gcompile_glib
;    without any argument.
;
;
PRO gcompile_glib, path
  COMPILE_OPT IDL2
  ON_ERROR, 0

  ;;If path is not given consider the path of current file
  IF (N_PARAMS() EQ 0) THEN $
     path = FILE_DIRNAME(ROUTINE_FILEPATH('gcompile_glib')) + PATH_SEP()

  PRINT, 'GCOMPILE glib : ' + path

  ;;Forward declaration of functions
  FORWARD_FUNCTION gfit_comp_Gauss
  FORWARD_FUNCTION gfit_comp_powerlaw
  FORWARD_FUNCTION gfit_comp_simplepar
  FORWARD_FUNCTION gfit_weighted_dev_gauss
  FORWARD_FUNCTION gfit_component
  FORWARD_FUNCTION gfit_free_param
  FORWARD_FUNCTION gfit_get_covar
  FORWARD_FUNCTION gfit_get_par
  FORWARD_FUNCTION ggp_get_state
  FORWARD_FUNCTION ggp_hist
  FORWARD_FUNCTION ggp_plot_struct
  FORWARD_FUNCTION ggp_prepare_data
  FORWARD_FUNCTION gpc
  FORWARD_FUNCTION gcfmt
  FORWARD_FUNCTION gsearch
  FORWARD_FUNCTION gdim
  FORWARD_FUNCTION gfexists
  FORWARD_FUNCTION gfloat
  FORWARD_FUNCTION ggauss
  FORWARD_FUNCTION ggen_delta
  FORWARD_FUNCTION ggen
  FORWARD_FUNCTION ghist
  FORWARD_FUNCTION gindices_array
  FORWARD_FUNCTION gisunix
  FORWARD_FUNCTION gloggen
  FORWARD_FUNCTION gminmax
  FORWARD_FUNCTION gn2base
  FORWARD_FUNCTION gn2hex
  FORWARD_FUNCTION gn2s
  FORWARD_FUNCTION gnan
  FORWARD_FUNCTION gn
  FORWARD_FUNCTION gproduct
  FORWARD_FUNCTION greadtexttable
  FORWARD_FUNCTION groutineexists
  FORWARD_FUNCTION gsearch
  FORWARD_FUNCTION gsize
  FORWARD_FUNCTION gstru_flatten
  FORWARD_FUNCTION gstru_sub
  FORWARD_FUNCTION gstru_tagnames
  FORWARD_FUNCTION gtype

  ;;List of .pro files to be compiled
  gcompile, /hold, path + 'glib_init.pro'

  gcompile, /hold, path + 'phys/gpc.pro'
  gcompile, /hold, path + 'utils/gassert.pro'
  gcompile, /hold, path + 'utils/gcfmt.pro'
  gcompile, /hold, path + 'utils/gdim.pro'
  gcompile, /hold, path + 'utils/gdist.pro'
  gcompile, /hold, path + 'utils/gfexists.pro'
  gcompile, /hold, path + 'utils/gfloat.pro'
  gcompile, /hold, path + 'utils/ggauss.pro'
  gcompile, /hold, path + 'utils/ggen_delta.pro'
  gcompile, /hold, path + 'utils/ggen.pro'
  gcompile, /hold, path + 'utils/ghist.pro'
  gcompile, /hold, path + 'utils/gindices_array.pro'
  gcompile, /hold, path + 'utils/gisunix.pro'
  gcompile, /hold, path + 'utils/gkey.pro'
  gcompile, /hold, path + 'utils/gloggen.pro'
  gcompile, /hold, path + 'utils/gminmax.pro'
  gcompile, /hold, path + 'utils/gn2base.pro'
  gcompile, /hold, path + 'utils/gn2hex.pro'
  gcompile, /hold, path + 'utils/gn2s.pro'
  gcompile, /hold, path + 'utils/gnan.pro'
  gcompile, /hold, path + 'utils/gn.pro'
  gcompile, /hold, path + 'utils/gprint_error.pro'
  gcompile, /hold, path + 'utils/gprint_mgr.pro'
  gcompile, /hold, path + 'utils/gprint.pro'
  gcompile, /hold, path + 'utils/gproduct.pro'
  gcompile, /hold, path + 'utils/gpro_par_key.pro'
  gcompile, /hold, path + 'utils/gps.pro'
  gcompile, /hold, path + 'utils/greadtexttable.pro'
  gcompile, /hold, path + 'utils/groutineexists.pro'
  gcompile, /hold, path + 'utils/gsearch.pro'
  gcompile, /hold, path + 'utils/gsize.pro'
  gcompile, /hold, path + 'utils/gstru_flatten.pro'
  gcompile, /hold, path + 'utils/gstru_insert.pro'
  gcompile, /hold, path + 'utils/gstru_sub.pro'
  gcompile, /hold, path + 'utils/gstru_tagnames.pro'
  gcompile, /hold, path + 'utils/gtype.pro'

  gcompile, /hold, path + 'gnuplot/ggp_init.pro'
  gcompile, /hold, path + 'gnuplot/ggp_clear.pro'
  gcompile, /hold, path + 'gnuplot/ggp_cmd.pro'
  gcompile, /hold, path + 'gnuplot/ggp_data.pro'
  gcompile, /hold, path + 'gnuplot/ggp_get_state.pro'
  gcompile, /hold, path + 'gnuplot/ggp_hist.pro'
  gcompile, /hold, path + 'gnuplot/ggp_plot.pro'
  gcompile, /hold, path + 'gnuplot/ggp_plot_struct.pro'
  gcompile, /hold, path + 'gnuplot/ggp_prepare_data.pro'
  gcompile, /hold, path + 'gnuplot/ggp.pro'
  gcompile, /hold, path + 'gnuplot/ggp_set_state.pro'
  gcompile, /hold, path + 'gnuplot/gplot.pro'

  gcompile, /hold, path + 'gfit/gfit_init.pro'
  gcompile, /hold, path + 'gfit/components/gfit_comp_example.pro'
  gcompile, /hold, path + 'gfit/components/gfit_comp_gauss.pro'
  gcompile, /hold, path + 'gfit/components/gfit_comp_powerlaw.pro'
  gcompile, /hold, path + 'gfit/components/gfit_comp_simplepar.pro'
  gcompile, /hold, path + 'gfit/data_type/gfit_teststat_gauss.pro'
  gcompile, /hold, path + 'gfit/data_type/gfit_weighted_dev_gauss.pro'
  gcompile, /hold, path + 'gfit/gfit_add_comp.pro'
  gcompile, /hold, path + 'gfit/gfit_add_data.pro'
  gcompile, /hold, path + 'gfit/gfit_add_expr.pro'
  gcompile, /hold, path + 'gfit/gfit_prepare_cmp.pro'
  gcompile, /hold, path + 'gfit/gfit_compile.pro'
  gcompile, /hold, path + 'gfit/gfit_component.pro'
  gcompile, /hold, path + 'gfit/gfit_eval.pro'
  gcompile, /hold, path + 'gfit/gfit_free_param.pro'
  gcompile, /hold, path + 'gfit/gfit_get_covar.pro'
  gcompile, /hold, path + 'gfit/gfit_get_par.pro'
  gcompile, /hold, path + 'gfit/gfit_plot_data.pro'
  gcompile, /hold, path + 'gfit/gfit_rebin.pro'
  gcompile, /hold, path + 'gfit/gfit_plot.pro'
  gcompile, /hold, path + 'gfit/gfit_plot_resid.pro'
  gcompile, /hold, path + 'gfit/gfit_report.pro'
  gcompile, /hold, path + 'gfit/gfit_restore.pro'
  gcompile, /hold, path + 'gfit/gfit_run.pro'

  ;;Compile the package
  gcompile

  ;;Initialize GLIB
  glib_init, path
END
