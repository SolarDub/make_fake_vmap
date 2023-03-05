#!/bin/bash

gfortran -J ./mod/ -c ./src/f90/modules/nrtype.f90 ./src/f90/modules/*.f90 ./src/f90/*.f90
gfortran *.o -o ./bin/make_fake_vmap.exe

echo 'Executable created.'
echo 'Enter ./bin/make_fake_vmap.exe to run.'
