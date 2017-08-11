#!/bin/bash

halp=$(cat $1)
stack exec copts graph $2 "$halp" > graph.dot
dot -Tpng graph.dot -o graph.png
