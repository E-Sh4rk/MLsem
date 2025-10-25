#!/bin/bash

time for i in {1..10}; do ./_build/default/src/bin/native.exe test.ml > /dev/null; done
