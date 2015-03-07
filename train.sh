#!/bin/bash

numsv=40
numimg=140
numslices=144
basedir="$1"
traindist="/home/theapp/pca/dist/build/train/train"


function train {
  $traindist "$basedir/$1/" $numsv $numimg
}

function progress {
  echo -n '['
  for j in $(seq 1 $(expr $1 / 2));
  do 
    echo -n "="
  done
  for j in $(seq 1 $(expr 72 - $1 / 2));
  do 
    echo -n " "
  done
  echo -n -e "]\r"
}

for i in $(seq 1 $numslices);
do
  trainout=$(train $i)
  echo $trainout
  progress $i
done
