#!/bin/bash

# Get the directory of the current script
DIR=$(dirname "$0")
echo "- Generating: $DIR."

#Rscript -e "setwd('./hdis-audition-statements/'); bookdown::render_book('index.Rmd', "bookdown::gitbook")"
Rscript -e "bookdown::render_book('index.Rmd', 'bookdown::gitbook')"
echo "- Website generated using bookdown package."


# Pushing to GITHUB
git add .
git commit -m "website automatic update"
git push
echo "- WEBSITE has been pushed to GITHUB."
