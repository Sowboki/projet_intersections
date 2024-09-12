#!/bin/bash

pdflatex ./rapport.tex && pdflatex ./rapport.tex

rm *.out
rm *.log
rm *.toc
rm *.aux

