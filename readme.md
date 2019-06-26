# Navr
PAckage to work with positioning and navigation data. 

[![Travis build status](https://travis-ci.org/hejtmy/navr.svg?branch=master)](https://travis-ci.org/hejtmy/navr)
[![Coverage status](https://codecov.io/gh/hejtmy/navr/branch/master/graph/badge.svg)](https://codecov.io/github/hejtmy/navr?branch=master)

## Description
Navr uses variety of processing and plotting functions which are useful in cognitive psychology research focusing on human navigation. It has functions to split and smooth paths, plot pointing data, plot speeds, remove "teleportations" or jittery movement. It has functions to work with independent body and head rotations for VR focused research.

## Basics
Navr builds an S3 `navr` object which is passed into most functions. Navr objech has a specific structure:
```r
navr_obj$data : data frame with sepcified columns (timestamp, position_x, position_y, rotation_x, fps, etc.)
navr_obj$area_boundaries <- list(x=c(0,100), y=c(-50,50), z=NULL)
```
This allows graphing and preprocessing to always work on valid data. Many functions are provided with generic footprint, allowing users to pass also vectors as parameters. For example function `calculate_distances(mat_xy)` can also be called on unprocessed navr object as `add_distances(navr_obj)`


