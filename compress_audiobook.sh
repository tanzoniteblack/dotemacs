#! /bin/zsh

# find -iname "*.mp3" | parallel ~/bin/compress_audiobook.sh

mkdir -p compressed
sox -G -v 0.90 $1 "compressed/$1" compand 0.3,5 6:-70,-60,-20 -10 -6 0.2 channels 1;
