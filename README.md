# Principal Component Analysis Via Singular Value Decomposition

A [#SmartCityHack](http://www.global.datafest.net/) project rolled by
[Alex](http://github.com/redxaxder),
[Daniel](http://github.com/friedbrice),
[Steven](http://github.com/StevenClontz),
and [Zack!](http://github.com/ZSarver)

Abstract: using a [live feed][5] provided by the City of Auburn,
we can programmatically determine if Auburn Tigers fans have begun
[rolling Toomer's Corner](https://github.com/IsToomersCornerBeingRolledRightNow/istoomerscornerbeingrolledrightnow.github.io) by analyzing images via Canny edge detection and analysis of gradient vectors.
These techniques rely on very particular geometric properties of the phenomenon we are trying to detect.
This repository instead seeks to accomplish the same via a more general image analysis algorithm, often termed Principal Component Analysis, in order to generalize the image-detection engine to various other real-world applications.

## Purpose

Our live implementation of [Is Toomer's Corner Being Rolled Right Now](http://istommerscornerbeingrolledrightnow.com) relies on Canny edge detection to preprocess images and then analyses the gradient vectors in the Canny-ized image in order to guess at whether Toomer's Corner is covered in celebratory toilet paper.

![unrolled corner](http://istoomerscornerbeingrolledrightnow.github.io/assets/roll0.jpg)

![rolled corner](http://istoomerscornerbeingrolledrightnow.github.io/assets/roll3.jpg)

The Canny-gradient algorithm reliably detects large numbers of near-vertical and vertical lines in an image, and thus provides a good guess at whether celebration is happening on the corner.
However, the limits of such an algorithm are apparent as soon as one seeks to generalize to the detection of other phenomenon--it is not clear that the geometric properties of a _usual_ image will be significantly different from those of a _unusual_ image.
In essence, we lucked out, simply because because of gravity, and Canny-gradient detection takes advantage of that position.

It became clear to us that if we wanted an image-detection algorithm that was useful outside of Auburn, we'd need something more general.
Our solution: we will train our algorithm to spot those differences through statistical analysis of a large set of usual images.

Using these methods, we hope to be able to detect arbitrary unusual images, which may then be flagged for analysis by a secondary image processor or for human intervention/action.

We use computationally-expensive [Principal Component Analysis](http://en.wikipedia.org/wiki/Principal_component_analysis) (abbreviated PCA) to generate training data representative of presumably usual images.
We may then use computationally-cheap image analysis methods to compare a live image to the training data, and determine whether the live image is usual or unusual.

## Summary of Files

#### ImageToVector.hs

Haskell source code.
Compiles to a library.

This library contains utility functions for loading, converting, and manipulating images.

#### PCA.hs

Haskell source code.
Compiles to library.

This library contains the functions needed to perform principal component analysis on vectorized data.

#### compare.hs

Haskell source code.
Compiles to an executable.

We're passed (1) the path to a directory that contains training data
and (2) the path to an image from the camera feed.
Chops up the image into 144 smaller 80 by 80 sectors, and calculates, for
each sector, the distance to
the linear-regression hyperplane for that sector, and divides by the
training data's average distance to the same hyperplane.
Returns, to `stdout`, the square sum of the scores described above.
Higher number means the image is more unusual.

#### extract.hs

BASH script.

We're passed (1) a directory that contains sample data and the results
of the training process and (2) the directory in which we want to
save only the results. This script will extract the results of training and place them in a seperate directory, preserving subdirectory structure.

#### genstats.hs

Haskell source code.
Compiles to executable.

We're passed a directory path `.../chopped/someNumber` containing
training data (presumably 'typical' images from the camera feed) and
the `hyperplane.txt` generated by `train.hs`.
We compare each image to the hyperplane, take the mean distance,
and save that in a text file, called `avgdist.txt`, in the same directory.
This completes analysis of the training data.

#### genstats.sh

BASH script wraper for `genstats.hs`

We're passed the path of the directory containing chopped images and 
hyperplanes. This script runs genstats on each of the 144 subdirectories,
so that each directory should end up containing `avgdist.txt`.

#### preconvert.hs

Haskell source code.
Compiles to executable.

We're passed the path to an image. We expect that image to be 1280 by
720, and we chop that image into 144 smaller 80 by 80 pieces, saving the
pieces with the same file name as the original but in numbered
subdirectories, `chopped/1`, `chopped/2`, etc.

#### preconvert.sh

BASH script wrapper for `preconvert.hs`

We're passed a directory that contains thousands of images. This
script runs preconvert on each image. Results should be 144 subdirectories
`chopped/1`, `chopped/2`, etc., each of which should contains thousands of tiny images.

#### train.hs

Haskell source code.
Compiles to executable.

We're passed (1) the path to a directory and (2) an integer.
Saves a hyperplane (dim = 2nd arg) that is the linear regression
of all of the images in the directory to hyperplane.txt in the same
directory.

#### `train.sh`

BASH script wrapper for `train.hs`

We're passed the path to a directory containing chopped images. This
script runs train on each subdirectory, which analyzes the images contained
inside and writes a file, hyperplane.txt, into that subdirectory.

## Method

The week leading up to the competition deadline, we began capturing images from  the Toomer's live web cam feed periodically.
We managed to collect roughly 10,000 still frames, stored as 1280 x 720 32-bit color `.png` files.

PCA relies on performing [singular value decomposition](http://en.wikipedia.org/wiki/Singular_value_decomposition) (abbreviated SVD) of the matrixized version of our training data (the so-called _principal components_ are those singular vectors corresponding to an arbitrary but fixed number of the largest singular values).
We first represent each image as a high-dimensional row vector (2,764,800 dimensions, one dimension for each x coordinate, y-coordinate, and color channel of each pixel).
We form the 10,000 by 2,764,800 matrix whose rows are the vectorized training images, and we perform the SVD in order to find the singular vectors.
These singular vectors, when shifted by the average of our training images, span the best-fit hyperplane to our training images.
We may then measure the unusualality of an image by calculating it's euclidean distance to the best-fit hyperplane.

![principal component analysis](https://raw.githubusercontent.com/IsToomersCornerBeingRolledRightNow/theScriptAndImagesForOurProjectVideo/master/13.png)

We quickly determined that this method, as described, was inadequate, particularly when the kernel complained about not having 6TB of RAM to allocate and terminated our training program before completion.

Our solution was twofold.

1. We decided to chop each 1280x720 image into 144 smaller 80x80 sectors, train and analyze each sector separately 

## Results


