;=====================================================================
;NAME:
;  gcompile_qsfit
;
;PURPOSE:
;  Initialize the 'qsfit' package.
;
;DEPENDENCIES:
;  gcompile (https://github.com/gcalderone/gcompile)
;
;PARAMETERS:
;  PATH (optional input, a scalar string)
;    Path to the qsfit directory.  If this parameter is not given 
;    the path where this routine is stored will be considered.
;
;USAGE:
;  To compile the 'qsfit' package you should either:
;
;  - copy the gcompile_qsfit.pro file in a directory listed in the
;    IDL_PATH environment variable and call:
;      gcompile_qsfit, 'PATH/TO/qsfit'
;
;  - copy the gcompile_qsfit.pro file in a directory NOT listed in the
;    IDL_PATH environment variable (say PATH/TO/qsfit), add:
;    the following line in one of your source files:
;      @PATH/TO/qsfit/gcompile_qsfit.pro
;    and then call:
;      gcompile_qsfit
;    without any argument.
;
;
PRO gcompile_qsfit, path
  COMPILE_OPT IDL2
  ON_ERROR, 0

  ;;If path is not given consider the path of current file
  IF (N_PARAMS() EQ 0) THEN $
     path = FILE_DIRNAME(ROUTINE_FILEPATH('gcompile_qsfit')) + PATH_SEP()

  PRINT, 'GCOMPILE qsfit : ' + path

  ;;Forward declaration of functions
  FORWARD_FUNCTION qsfit_comp_emline
  FORWARD_FUNCTION qsfit_comp_galaxytemplate
  FORWARD_FUNCTION qsfit_comp_ironoptical
  FORWARD_FUNCTION qsfit_comp_ironuv_prepare
  FORWARD_FUNCTION qsfit_comp_ironuv
  FORWARD_FUNCTION qsfit_comp_sbpowerlaw
  FORWARD_FUNCTION ggaltempl_mannucci01
  FORWARD_FUNCTION ggaltempl_swire
  FORWARD_FUNCTION qsfit_version
  FORWARD_FUNCTION qsfit_line_coverage
  FORWARD_FUNCTION qsfit_estimate_fwhm_voff
  FORWARD_FUNCTION qsfit_reduce_line_templ
  FORWARD_FUNCTION qsfit_reduce_line
  FORWARD_FUNCTION qsfit_line_quality_meaning
  FORWARD_FUNCTION qsfit_cont_quality_meaning
  FORWARD_FUNCTION qsfit_iron_quality_meaning
  FORWARD_FUNCTION qsfit_galaxy_quality_meaning
  FORWARD_FUNCTION qsfit_flatten_results
  FORWARD_FUNCTION qsfit_input
  FORWARD_FUNCTION qsfit

  ;;List of .pro files to be compiled
  gcompile, /hold, path + 'components/qsfit_comp_emline.pro'
  gcompile, /hold, path + 'components/qsfit_comp_galaxytemplate.pro'
  gcompile, /hold, path + 'components/qsfit_comp_ironoptical.pro'
  gcompile, /hold, path + 'components/qsfit_comp_ironuv.pro'
  gcompile, /hold, path + 'components/qsfit_comp_sbpowerlaw.pro'
  gcompile, /hold, path + 'components/qsfit_comp_balmer.pro'
  gcompile, /hold, path + 'components/ggaltempl_mannucci01.pro'
  gcompile, /hold, path + 'components/ggaltempl_swire.pro'
  gcompile, /hold, path + 'qsfit.pro'

  ;;Compile the package
  gcompile
  qsfit_prepare_options

  PRINT, 'QSFIT version: ' + qsfit_version()
END
