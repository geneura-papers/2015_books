#!/bin/sh

rm *.aux
rm *.bbl
rm *.blg
rm *.log
rm *~
rm *.dvi

latex paper.tex && latex paper.tex &&  latex paper.tex 
bibtex paper
latex paper.tex && latex paper.tex && latex paper.tex

echo "-----------------------------" 

dvips paper.dvi -o && ps2pdf paper.ps

rm *.ps

echo "-----------------------------"
