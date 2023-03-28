#!/bin/bash
pdflatex main.tex
while inotifywait -r -e modify .; do
    clear
    pdflatex main.tex
done
