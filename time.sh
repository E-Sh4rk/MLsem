#!/bin/bash

time for i in {1..10}; do ./_build/default/src/main/prototype.exe > /dev/null; done
