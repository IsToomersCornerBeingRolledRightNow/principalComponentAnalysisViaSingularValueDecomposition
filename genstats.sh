#!/bin/bash

# we're passed the dir path .../chopped, which contains 144 folders, each with 10k images and a hyperplane.txt inside
# we want to run genstats.hs on each of the 144 dirs
# that's about it, could just be a one-liner

basedir="$1"
numslices=144
gendist="/home/theapp/pca/dist/build/genstats/genstats"

function genstats {
  $gendist "$basedir/$1/"
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
  out=$(genstats $i)
  clearline
  echo -e "$i:\t $out"
  progress $i $numslices
done
