#! /usr/bin/env bash

for var in "$@"
do
  filename=$(basename -- "$var")
  extension="${filename##*.}"
  filename="${filename%.*}"
  id3v2 -t "$filename" "$var"
done
