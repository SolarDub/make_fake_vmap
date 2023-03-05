# make_fake_vmap

This repository is currently for demonstration purposes only.

This program develops 3-component (toroidal, poloidal, radial) velocity maps of convection-like (supergranulation) 
and axisymmetric (rotation and meridional circulation) flows manifesting at the solar surface.

The solar rotation is directed toroidally (i.e. longitudinally) with a latitudinal variation profile. 
The meridional circulation is directed poloidally (i.e. latitudinally), also with a latitudinal variation profile.

The supergranulation flows have a weak radial flow (upward perpendiclar to surface) and strong divergent flow (horizontal to surface).

Each flow is described by spherical-harmonic spectral coefficients which are used as spectral amplitudes for orthongonal sets of spherical-harmonic functions.

This program is a FORTRAN90 adaptation of early FORTRAN77 data-simulations written by David Hathaway, as described in his 1988 Solar Physics paper 
([Hathwaway (1988)](https://ui.adsabs.harvard.edu/abs/1988SoPh..117..329H/abstract)), which culminated in the production of simulated solar Doppler image data as
observed by the Michelson Doppler Imager ([MDI](http://soi.stanford.edu/)) aboard the Solar and Heliospheric Observatory ([SoHO](https://soho.nascom.nasa.gov/sindex.html)).

## Prequisities

To compile and link the program requires GFortran to be installed. You may wish to use another compiler to install, but I have only tested with GFortran (on MacOS).
Binary files for the installation of gfortran are available [here](https://gcc.gnu.org/wiki/GFortranBinaries).

To view the component maps, and to make horizontal velocity quiver maps, two Python programs have been made available which require the installation of Python3.7 or higher.

## Compiling and Linking with GFortran

In lieu of a proper Makefile, a simple script, mkfile.sh, is currently used to compile and link the program. In the base make_fake_vmap directory, type:

    $ ./mkfile.sh

It is then advisable to remove the object files:

    $ rm *.o
       
An executable file, 
 
 ## 

