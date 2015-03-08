#!/bin/bash

# We're passed a directory that contains thousands of images. This
# script runs preconvert on each image. Results should be 144 subdirs
# chopped/1, chopped/2, ..., each of which should contains thousands
# images.

dir="$1"
preconvertdist="/home/theapp/pca/dist/build/preconvert/preconvert"
filelist=$(find "$dir" -maxdepth 1 -iname "*.png")
numfiles=$(echo "$filelist" | wc -l)

function preconvert {
  $preconvertdist "$1"
}

function format {
  date -u -d @"$1" +"%T"
}

starttime=$(date +%s)
function progress {
  current=$1
  max=$2
  elapsed=$(expr $(date +%s) - $starttime)
  width=78
  let numeq=current*width/max
  let numspace=width-numeq
  let estimated_remaining=max*elapsed/current-elapsed
  #elapsed/total = current/max
  #total = max * elapsed / current
  echo -n '['
  for j in $(seq 1 $numeq);
  do 
    echo -n "="
  done
  for j in $(seq 1 $numspace);
  do 
    echo -n " "
  done
  echo -n -e "]\r"
  echo -n "[$(format $elapsed)/$(format $estimated_remaining)]"
}

function clearline {
  echo -n -e "\r"
  for z in $(seq 1 80)
  do
    echo -n ' '
  done
  echo -n -e "\r"
}


i=0
for f in $filelist
do
  out=$(preconvert $f)
  clearline
  echo "$out"
  let i=i+1
  progress $i $numfiles
done
