#!/bin/bash

gfortran -J ./mod/ -c ./src/f90/modules/nrtype.f90 ./src/f90/modules/*.f90 ./src/f90/*.f90
gfortran *.o -o ./bin/mdi_fake_build_f90.exe

echo 'Executable created.'
echo 'Enter ./bin/mdi_fake_build_f90.exe to run.'
