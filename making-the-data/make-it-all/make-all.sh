#!/bin/bash

# around 5 minutes
./make-sierra-derivatives.R

# around 2.5 minutes
./make-recap-derivatives.R

# around 2 minutes
./make-ezproxy-derivatives.R

# just rsync-ing
./copy-scan-and-deliver-derivatives.sh

