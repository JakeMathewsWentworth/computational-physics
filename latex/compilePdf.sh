#!/bin/bash

latex $1.tex
latex $1.tex
latex $1.tex
dvips -o $1.ps $1.dvi
ps2pdf $1.ps
evince $1.pdf &

rm $1.ps $1.dvi $1.log $1.aux
