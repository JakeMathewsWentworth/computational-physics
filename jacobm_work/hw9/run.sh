#! /bin/bash


rm *.dat

gfortran sho.f90
./a.out
xmgrace exact2.dat &
xmgrace approximation2.dat &
xmgrace energy2.dat &

