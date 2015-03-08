#!/bin/bash

# We're passed the path to a directory containing chopped images. This
# script runs train on each subdir, which analyzes the images contained
# inside and writes a file, hyperplane.txt, into that subdir.

basedir="$1"
numsv=$2
numimg=$3 #optional
numslices=144
traindist="/home/theapp/pca/dist/build/train/train"

function train {
  $traindist "$basedir/$1/" $numsv $numimg
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

for i in $(seq 1 $numslices);
do
  out=$(train $i)
  clearline
  echo $out
  progress $i $numslices
done
