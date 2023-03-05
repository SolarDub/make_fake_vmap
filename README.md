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

In lieu of a proper Makefile, a simple script, mkfile.sh, is currently used to compile and link the program. In the base make_fake_vmap directory, enter:

    $ ./mkfile.sh

It is then advisable to remove the object files:

    $ rm *.o
       
An executable file, make_fake_vmap.exe is produced in the /bin directory
 
## Execution

To execute the program, enter:

    $ ./bin/make_fake_vmap.exe

The execution proceeds by reading in convection spectral amplitudes (from /inputspec/fakespec.txt) and writing those pertaining to the first 20 spherical-hamonic degrees, l, in three columns: radial, poloidal, toroidal.

The global (spherical harmonic order, m = 0) toroidal rotation and poloidal meridional circulation spectra are calculated, followed by a calculation of the convection
spectrum (radial and poloidal) for all spherical harmonic degrees, l, and orders, m, from l = 1 to lmax and m = 1 to l. (Where lmax = nx - 2, nx being the number of latitudinal pixels (set in src/modules/mod_params.f90), which is currently set at 512. To simulate MDI images, this can be changed to 1024.)

On calculating the three spectra (radial, poloidal, toroidal), the program calculated the respective velocities across longitundial strips over a set of nx/2 hemispheric latitudes. This is performed by combining the calculated spectral amplitudes with the spherical harmonic functions at each latitude and performing an inverse fourier transform to produce the 3-component longitudinal velocity profiles. Symmetries of the spherical harmonics across the equator means that the southern-latitude longitudnal strips can be calculated at the same time as the northern-latitude counterparts.

The northern and southern hemisphere profiles are then compiled into a single map for each component and these maps are written to file with u, v and w prefixes relating to toroidal, polidal and radial velocities, resepectively.

Pre-made map files have been provided in the /output directory and are named as follows:
- u, v and w relate to toroidal, polidal and radial velocity maps, respectively,
- number prefix 1 relates to maps that include all physical velocity components (i.e. axisymmetric flows + supergranulation),
- number prefix 5 relates to maps that include axisymmetric flows only,
- number prefix 9 relates to maps that include supergranule flows only.

These files may be viewed using the provided Python programs:
- plot_map.py plots velocity maps for all three components
- plot_hv_map.py plots velocity maps for the horizontal components (toroidal and poloidal) plus a horizontal velocity quiver map of the central 64-by-64 square-pixel region.

## Velocity Maps

Below are plotted the velocity maps for the three components: toroidal, polidal and radial. Note that the maps represent only the longitudinal domain that would be visible to an observer (i.e. the visible face of the Sun), culminating in a map with nx-by-nx dimensions (with nx currently set to 512). For each plot, below the map 
is a near-equator (0.176 degrees S), longitundinal cross-section velocity profile, while to the right is a near meridian (0.176 degrees E - E/W point 'backwards' when observing the Sun) latitudinal cross-section velocity profile.

While maps of all three components are produced, a discussion of radial map (weak radial supergranule upflows) will be skipped for the more interesting toroidal and polidal maps.

Each component map can be expressed as a sum of an axisymmetric flow map and a convection flow map. For the east-west toroidal flow, this means a summation of the longitudinally directed rotation and the longitudinal component of the supergranulation flows. Conversely, the north-south poloidal flow is a summation of the latitudinally directed meridional circulation and the latitudinal component of the supergranulation flows.

In the three pairs of images below are:

1. Axisymmetric flows:  (Left) Rotation profile,        (Right) Meridional Circulation profile
2. Supergranulation:    (Left) East-West component,     (Right) North-South component
3. Summed flows:        (Left) East-West velocities,    (Right) North-South velocities

Notes:

1. The latitudinal-dependent profiles are seen in the north-south cross-sections to the right of the map.
2. The supergranulation profiles are seen in both cross-sections as structured fluctations.
3. The rotation dominates the toroidal map and the merdional circulation profile is barely seen below the supergranulation signal

<img src="https://user-images.githubusercontent.com/81772405/222982231-cac5de0f-2bb5-441b-915f-5af2ea8c171d.jpg" width="500" />                  <img src="https://user-images.githubusercontent.com/81772405/222982234-f6d8555a-b430-427a-9adb-19fbe5ac5bc2.jpg" width="500" />

<img src="https://user-images.githubusercontent.com/81772405/222982289-4020835e-b000-483d-967a-f4b9e1e4a059.jpg" width="500" />                  <img src="https://user-images.githubusercontent.com/81772405/222982293-92ce387f-9449-48f9-a5e9-dc50dbd867f4.jpg" width="500" />

<img src="https://user-images.githubusercontent.com/81772405/222982588-cbe887c2-83db-4690-a4ef-055a330ddb39.jpg" width="500" />                  <img src="https://user-images.githubusercontent.com/81772405/222982591-d2c8cca7-2f5f-40de-aee5-0b0bd9c4795d.jpg" width="500" />


## Cellular Supergranulation Velocity Profiles

While the axisymmetric flows can be clearly seen on the global maps, the supergranulation signals are much smaller. They are best observed by selecting a central region of the map, in this case a 64-by-64 pixel square. The toroidal and poloidal maps are then combined into a single horizontal vector velocity quiver map, as shown below. The supergranulation flows can be seen as cellular structures situated across the region.

<img src="https://user-images.githubusercontent.com/81772405/222983524-a8195b8c-406b-4cdd-a8f9-ec5112cd60f3.jpg" width="600" />



