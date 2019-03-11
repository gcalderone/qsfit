# QSFit

**QSFit** (Quasar Spectral Fitting) is a software package written in IDL to automatically perform the analysis of Active Galactic Nuclei (AGN) optical spectra. The software provides luminosity estimates for the AGN continuum, the Balmer continuum, both optical and UV iron
blended complex, host galaxy and emission lines, as well as width, velocity offset and equivalent width of 20 emission lines (Halpha, Hbeta, Mgii, [Oiii], Civ, etc.).  The ultimate purpose of **QSFit** is to allow astronomers to run standardized recipes to analyze the AGN
data, in a simple, replicable and shareable way. The whole fitting process is customizable for specific needs, and can be extended to analyze spectra from other data sources.

The main website is: [http://qsfit.inaf.it](http://qsfit.inaf.it).

If you find QSFit useful please cite the refernce paper: [Calderone et al. 2017](http://adsabs.harvard.edu/abs/2017MNRAS.472.4051C) (also available on [arXiv](
https://arxiv.org/abs/1612.01580)).


## Usage

To use QSFit you need IDL (ver >= 8.1) and Gnuplot (ver. >=5.0) to be installed on your system.

Start an IDL session and change to the directory where you downloaded the QSFit files, e.g.:
```idl
CD, 'C:\the\path\where\qsfit\code\is\located'  ;  on windows
```
or
```idl
CD, '/the/path/where/qsfit/code/is/located'   ; on GNU/Linux
```

## Code compilation

Before using QSFit you need to compile its code and and dependencies:
```idl
compile
```

The `compile` procedure and QSFit routines **can not** be used within the same procedure.  Best practice is to call `compile` manually, at the very beginning of the IDL session.

There is no need to modify your `IDL_PATH` settings to use QSFit.

## Download the spectra FITS files:

currently only SDSS DR10 FITS files are supported by QSFit.  These
files can be downloaded from:
http://skyserver.sdss.org/dr10/en/tools/explore/obj.aspx

click, on *Search*, enter search cirteria and click on *Go*.  Verify the resulting object is the one you're interested in and click on *FITS* (in the middle of the column on the left), and on *Download*.

Alternativaly you may copy and paste an URL similar to the following:

http://dr10.sdss3.org/sas/dr10/sdss/spectro/redux/26/spectra/0752/spec-0752-52251-0323.fits

directly on the web browser.  The numbers to be tweaked are:
- the plate ID (0752 in the example above. NOTE: this number appears twice in the URL!);
- the MJD (52251 in the example above);
- the fiber ID (0323 in the example above);

The QSFit package comes already with one such file in the `data` directory.  This can be used to quickly test the QSFit procedures (see below).

## Run QSFit analysis

Enter the following commands:
```idl
in = qsfit_input('data/spec-0752-52251-0323.fits', type='SDSS_DR10', z=0.3806, ebv=0.06846)
res = qsfit(in)
```

Parameters are:
 - the path to a SDSS-DR10 FITS file;
 - the redshift of the source;
 - the color excess;

The results are returned in the `res` variable.

If you want the results to be saved in a directory (which must have been previously created) use the `OUTNAME` keyword, e.g.
```idl
res = qsfit(in, outname='output/MySavedFile')
```

This will save all the logs in a file named `output/MySavedFile_QSFIT.log` and the results in `output/MySavedFile_QSFIT.dat`.  The latter contains a binary representation of the results, and can be used to restore those data in IDL.  Note that if you run again the above command the program will stop immediately with a message: `File output/MySavedFile_QSFIT.dat already exists`.

This is not an error, it simply says that the analysis has already been performed and there is no need to run it again.  To actually re-run the analysis you should manually delete that file.

## Plot the results:
```idl
qsfit_plot, res
```

To plot the data rebinned by a factor of 5:
```idl
res.gfit.obs.(0).plot.rebin = 5
qsfit_plot, res
```

To save the gnuplot files in a specific path:
```idl
qsfit_plot, res, filename='output/MySavedFile'
```

Note that the directory "output" must have been previously created and that the file name must not have any extension.  Two files will be created: one with the `.gp` extension and one with the `_resid.gp` suffix.  The former is the actual plot, to compare the data and the model.  The latter is the plot of the residuals.

These are just gnuplot files, and IDL is not needed to handle them. You can simply load them into gnuplot for the gnuplot shell:
```
gnuplot> load 'output/MySavedFile.gp'
```
or modify them for to suite your goal.  If you use a modern terminal for gnuplot, such as `wxt`, you may use the "Export to plot file" button to obtain a pdf copy.

Alternatively you may modify the plot files by changing the line:
```
set term wxt
```
with
```
set term pdf
set output 'filename.pdf'
```
and reload the file in gnuplot.

## Monte Carlo resampling:

To run the Monte Carlo reampling method (see corresponding section in the reference paper) simply use the `RESAMPLE=` keyword in the qsfit call, e.g.:
```idl
res = qsfit(in, resample=10)
```
The `res` variable will be an array of 1000 elements (or whatver number you set with the `RESAMPLE` keyword) where the first is the actual analysis on the real data, while the remaining ones will be the analysis on the mock data generated from the best fit model of the actual analysis.

## QSFit Customization:
Some details of QSFit behavior can be customized through the `!QSFIT_OPT` global variable.  Check the `qsfit_prepare_options` procedure for a list of default values and their meaning.

For instance to disable the Balmer continuum component set:
```idl
!qsfit_opt.balmer = 0
```
Or to 1 to re/enable it

To add absorption lines at rest frame wavelengths 2705A and 3810A set:
```
!qsfit_opt.abslines_wavelengths = '2705, 3810'
```

To use a Lorentzian profile for the emission line (rather than a Gaussian one) set:
```idl
!qsfit_opt.lorentzian = 1
```

Further customization can be performed by modifying the source code in `qsfit.pro`.  Check the documentation in the reference paper and in the code itself.


## Fitting multiple datasets:

Starting from v1.3.0 QSFit allows to analyze multiple data sets simultaneously.  E.g. to consider both the optical spectrum and photometry from SDSS:
```idl
in1 = qsfit_input('data/spec-0752-52251-0323.fits', type='SDSS_DR10', z=0.3806, ebv=0.06846)
in2 = qsfit_input('data/photometry.txt', type='ASCII', z=0.3806, ebv=0.06846)
res = qsfit(LIST(in1, in2))
```
Note that we used type='ASCII' to read the text file `data/photometry.txt` which contains ASCII data in three (space
separated) columns: the wavelength (observer frame, in Angstrom), the observed flux and uncertainties (in units of 10-17 erg s^-1 cm^-1 A^-1).

Alternatively the data can be provided directly as numerical arrays (with the same units as above), e.g.:
```idl
in1 = qsfit_input('data/spec-0752-52251-0323.fits', type='SDSS_DR10', z=0.3806, ebv=0.06846)
wavelength = [3588.52,4862.24,6289.31,7712.08,9230.77]
flux = [14.6768,8.92081,6.38598,4.82714,4.85875]
uncert = [0.391382,0.148469,0.150169,0.0933146, 0.147523]
in2 = qsfit_input(wavelength, flux, uncert, z=0.3806, ebv=0.06846)
res = qsfit(LIST(in1, in2))
```

To plot the results we can assign individual labels and plotting
options to each dataset, e.g.:
```idl
res.gfit.obs.(0).data.(0).plot.label = 'Spectrum'
res.gfit.obs.(0).data.(1).plot.label = 'Photometry'
res.gfit.obs.(0).data.(1).plot.gp = 'pt 7 ps 2 lt rgb "dark-green"'
qsfit_plot, res
```
