#!/bin/sh

rm *.aux
rm *.bbl
rm *.blg
rm *.log
rm *~
rm *.dvi

echo "-------------Compiling Paper----------------"

latex paper.tex && latex paper.tex &&  latex paper.tex 
bibtex paper
latex paper.tex && latex paper.tex && latex paper.tex

dvips paper.dvi -o && ps2pdf paper.ps

rm *.ps
rm *.dvi


echo "-------------Compiling Responses----------------"

pdflatex Response\ to\ reviewers.tex 

echo "-----------------------------"

