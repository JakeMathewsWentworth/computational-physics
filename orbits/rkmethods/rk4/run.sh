#! /bin/bash

gfortran betterorbits.f90
echo 0.003 | ./a.out
xmgrace xy-posrk4.dat &
