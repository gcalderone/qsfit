GLIB - Giorgio's IDL library
(Giorgio Calderone, giorgio.calerone@gmail.com)


**Introduction**

GLIB is part of my personal IDL library developed during my research
activities at Universita` degli Studi di Milano-Bicocca, INAF-OAB and
INAF-OAT.

The main facilities provided by GLIB are :

 - GGP: use gnuplot as backend for plotting;
 - GFIT: non-linear least squares fitting framework;

The former allows to plot using the "gnuplot" (ver. >= 5.0) external
program instead of using the IDL's builtin graphics system (see the
"gnuplot" directory for further informations, and the
"gnuplot/usage.pro" file for a few examples).

The latter is a general purpose, non-linear least squares fitting
framework based on the MPFIT minimization routine by Craig Markwardt.
It has many interesting features such as: handling of both Gaussian
and Poisson data, simplified data grouping/rebinning, possibility to
freeze/limit/tie each model parameter, availability of several model
components, and easy implementation of further components, etc... (see
the "gfit" directory for further informations, and the "gfit/examples"
directory for a few examples).

Beyond the GGP and GFIT packages, the GLIB library provides several
miscellaneous facilities used within the library, but potentially of
general interest.


**IMPORTANT NOTE:**

The software is provided "as is" without warranty of any kind.  In
particular, note that the development of GLIB is ongoing, hence you
may still encounter bugs or inconsistencies: USE IT AT YOUR OWN RISK!

If you want to signal a bug, contribute a solution, propose a
modification or collaborate in any way to the GLIB development and/or
to its documentation, feel free to send an email to:
giorgio.calderone@gmail.com


**Coding style**

Here I report the main coding style rules followed in developing GLIB:

- To use the GLIB library simply copy the whole directory structure in
  a directory accessible throught the IDL_PATH environment variable.

- The GLIB library may also compile itself using the "gcompile.pro"
  procedure (see https://github.com/gcalderone/gcompile).  The code to
  compile GLIB can be found in "gcompile_glib.pro";

- GLIB provides both "user callable" and "internal" routines.  The
  former are supposed to be called directly by the user, the latter
  should onlyd be called by GLIB routines.  The former are defined in
  a .pro file whose name is the same as the routine name, while the
  latter are defined in the same .pro file of the GLIB routine which
  uses it.

- All IDL reserved words are written in uppercase letters;

- All routines names begin with the letter "g".

- A routine parameter whose name starts with "_" indicates the
  parameter will be copied into a local scope variable before being
  used.  This is required when the parameter should be processed
  before being used in the routine, but we don't want the changes to
  be sent back to the caller routine;

- Many routines have a companion usage_ROUTINE_NAME procedure showing
  its basic usage and functionalities.  This procedure can be found in
  the usage.pro file within the directory of the main routine.

- All routines set ON_ERROR at their beginning according to the value
  of the !glib.on_error system variable.  In particular:

    !glib.on_error = 2: an error is propagated upwards in the call
                        stack until a CATCH block handles it or until
                        the main level is reached;

    !glib.on_error = 0: the execution will stop at the line causing
                        the error;

  Users should set the !glib.on_error value before using the glib's
  routines.  Changing its value during use may not result in the
  desired behaviour.

- The last error message, !ERROR_STATE structure and the call stack
  state when the last error occurred can be displayed using the
  gprint_error procedure, regardless of the value of !glib.on_error.
  Printing occur through "gprint, /override", hence the messages are
  reported on both standard output and currently opened log file (if
  any, see documentation of gprint_mgr).



**License**
Copyright (C) 2015,2017 Giorgio Calderone

This program is free software; you can redistribute it and/or
modify it under the terms of the GNU General Public License
as published by the Free Software Foundation; either version 2
of the License, or (at your option) any later version.

This program is distributed in the hope that it will be useful,
but WITHOUT ANY WARRANTY; without even the implied warranty of
MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
GNU General Public License for more details.

You should have received a copy of the GNU General Public License
along with this program; if not, write to the Free Software
Foundation, Inc., 51 Franklin Street, Suite 500, Boston, MA  02110-1335, USA.

See the LICENSE file for more information regarding the GNU General
Public License.



