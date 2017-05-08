#!/bin/bash

INPUT=MK

for d in $INPUT; do
    echo "Start with input:" $d
    #ls jpg/$d/*.jpg
    ffmpeg -safe 0 -r 1/5 -re -f concat -i <(printf "file '$PWD/%s'\n" $(ls jpg/$d/*.jpg)) -c:v libx264 -r 30 -pix_fmt yuv420p -vf "scale=trunc(iw/2)*2:trunc(ih/2)*2" -loglevel debug output/$d.mp4;
done
