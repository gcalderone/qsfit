@IDL/gcompile.pro
@IDL/Glib/gcompile_glib.pro
@IDL/qsfit/gcompile_qsfit.pro

PRO compile
  ;;General settings
  !QUIET=1
  !EXCEPT=1
  ;;!WARN.OBS_ROUTINES = 1
  ;;!WARN.OBS_SYSVARS = 1
  ;;!WARN.PARENS = 1

  ;;Compile dependencies
  path = 'IDL/Contrib/'

  ;;Forward declaration of functions
  FORWARD_FUNCTION cgErrorMsg
  FORWARD_FUNCTION FXMOVE
  FORWARD_FUNCTION gettok
  FORWARD_FUNCTION mpfit_call_func_noextra
  FORWARD_FUNCTION mpfit_call_func_extra
  FORWARD_FUNCTION mpfit_call_pro_noextra
  FORWARD_FUNCTION mpfit_call_pro_extra
  FORWARD_FUNCTION mpfit_call
  FORWARD_FUNCTION mpfit_fdjac2
  FORWARD_FUNCTION mpfit_enorm
  FORWARD_FUNCTION mpfit_lmpar
  FORWARD_FUNCTION mpfit_covar
  FORWARD_FUNCTION mpfit_revision
  FORWARD_FUNCTION mpfit_parse_version
  FORWARD_FUNCTION mpfit_min_version
  FORWARD_FUNCTION mpfit
  FORWARD_FUNCTION mrd_dofn
  FORWARD_FUNCTION mrd_chkfn
  FORWARD_FUNCTION mrd_unsigned_offset
  FORWARD_FUNCTION mrd_chkunsigned
  FORWARD_FUNCTION mrd_unsignedtype
  FORWARD_FUNCTION mrd_version
  FORWARD_FUNCTION mrdfits
  FORWARD_FUNCTION mrd_struct
  FORWARD_FUNCTION SXPAR
  FORWARD_FUNCTION valid_num

  ;;List of .pro files to be compiled
  gcompile, /hold, path + 'ccm_unred.pro'
  gcompile, /hold, path + 'cgerrormsg.pro'
  gcompile, /hold, path + 'cosmo_param.pro'
  gcompile, /hold, path + 'euler.pro'
  gcompile, /hold, path + 'fxpar.pro'
  gcompile, /hold, path + 'fxparpos.pro'
  gcompile, /hold, path + 'fxaddpar.pro'
  gcompile, /hold, path + 'fxmove.pro'
  gcompile, /hold, path + 'fxposit.pro'
  gcompile, /hold, path + 'gettok.pro'
  gcompile, /hold, path + 'lumdist.pro'
  gcompile, /hold, path + 'match.pro'
  gcompile, /hold, path + 'mpfit.pro'
  gcompile, /hold, path + 'mrdfits.pro'
  gcompile, /hold, path + 'mrd_hread.pro'
  gcompile, /hold, path + 'mrd_skip.pro'
  gcompile, /hold, path + 'mrd_struct.pro'
  gcompile, /hold, path + 'qsimp.pro'
  gcompile, /hold, path + 'remchar.pro'
  gcompile, /hold, path + 'repchr.pro'
  gcompile, /hold, path + 'setdefaultvalue.pro'
  gcompile, /hold, path + 'sxpar.pro'
  gcompile, /hold, path + 'trapzd.pro'
  gcompile, /hold, path + 'valid_num.pro'
  gcompile, /hold, path + 'zparcheck.pro'
  gcompile, /hold, path + 'compare_struct.pro'

  ;;Compile GLIB and QSFIT packages
  gcompile_glib
  gcompile_qsfit

  ;;Check all routines are resolved
  RESOLVE_ALL, /CONTINUE_ON_ERROR
END
