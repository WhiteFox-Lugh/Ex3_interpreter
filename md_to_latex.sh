#!/bin/sh
pandoc report_2.md -o report_2.pdf --latex-engine=lualatex -V documentclass=ltjarticle -V geometry:margin=1in
