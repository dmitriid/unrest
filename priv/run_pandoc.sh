#!/bin/sh

STRING=$1

cat "$STRING" | pandoc -f markdown -t html
