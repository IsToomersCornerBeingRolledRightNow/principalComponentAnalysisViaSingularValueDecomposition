#!/bin/bash

# We're passed (1) a directory that contains sample data and the results
# of the training process and (2) the directory in which we want to
# save only the results. This script will extract the results and place
# them in a seperate directory, preserving subdirectory structure.

sourceDir=$1
targetDir=$2

if [[ -d $targetDir ]]; then
  echo "Target directory exists. Aborting."
  exit
fi

mkdir ./targetDir

for i in $(seq 1 144); do
  mkdir ./targetDir/$i
  cp $sourceDir/$i/hyperplane.txt $targetDir/$i/hyperplane.txt
  cp $sourceDir/$i/avgdist.txt $targetDir/$i/avgdist.txt
done

echo "Training data copied to $targetDir"

