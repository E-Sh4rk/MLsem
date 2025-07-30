#!/bin/bash

time for i in {1..10}; do ./_build/default/src/bin/native.exe > /dev/null; done
